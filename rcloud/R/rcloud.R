#' Executes a function remotely on the PiCloud infrastructure
#' Requires that rcloud.setkey has first been called
#'
#' @param function.name The function to execute remotely
#' @param args The arguments to the function as a named list
#' @param uid A unique uid specific to a user's account
#' @param packages a vector containing any packages required
#' @param repos a list of repositories to install packages from if they are not available
#' @globals a named list containing any required global variables
#' @uid uid override (preferred behavior is to set environmental value)
#' @n Bunching factor. Probably only useful for the map function
rcloud.call <- function(function.name, args = list(), packages = c(), 
                        repos = getOption("repos"), globals = list(), uid = NULL, n = 1)
{
  # Serialize the arguments in ASCII form. To post to RCurl we must convert to a
  # character vector (it fails for raw vectors) so enforce ASCII to avoid misplaced
  # null characters. Finally place it in a multipart form container so it is treated
  # as a binary argument by PiCloud. Note that the filename field must be set to
  # an empty string or PiCloud will ignore the argument.
  args.serialized <- serialize(args, NULL, ascii = TRUE) 
  args.serialized <- rawToChar(args.serialized)
  args.serialized <- fileUpload("", contents = args.serialized)

  # Serialize the function itself
  f.serialized <- rawToChar(serialize(function.name, NULL, ascii = TRUE))
  f.serialized <- fileUpload("", contents = f.serialized)

  packages <- list(names = packages, repos = repos)
  packages.serialized <- rawToChar(serialize(packages, NULL, ascii = TRUE))
  packages.serialized <- fileUpload("", contents = packages.serialized)

  # Serialize the global variables
  globals.serialized <- rawToChar(serialize(globals, NULL, ascii = TRUE))
  globals.serialized <- fileUpload("", contents = globals.serialized)

  # Serialize the bunching factor
  n.serialized <- rawToChar(serialize(n, NULL, ascii = TRUE))
  n.serialized <- fileUpload("", contents = n.serialized)
  
  result <- rcloud.rest.call("rwrapper", uid, params = 
                             list(r_filename = "\"exec.R\"", 
                                  arg = args.serialized,
                                  f = f.serialized,
                                  packages = packages.serialized,
                                  globals = globals.serialized,
                                  n = n.serialized
                                  ))

  if(names(result)[1] == "error") warning( paste("Server Error Msg: ", result$error$msg) )
  
  result$jid
}

# partial submit -- timing of the initial results
# automatically determine bunching factor
rcloud.map <- function(function.name, args = list(), packages = c(), 
                        repos = getOption("repos"), globals = list(), uid = NULL,
                        Nmin = 10, Nsend = 5, Twait = 60, alpha = 1.2)
{
  if(Nsend > Nmin) {
    error("Nmin must be greater than or equal to Nsend.")
    return()
  }
  # If there are less than 10 arguments just naively run everything
  if(length(args) < Nmin) {
    sapply(args, function(x) rcloud.call(function.name, x, uid))

  # Otherwise try a fancier approach, batching together jobs if they are small
  } else {
    # Run with just the first Nmin arguments to gague job length
    # Depending on the time it takes for each job it may improve performance to batch
    # individual jobs together
    # We send Nmin jobs as a sample to gague ideal performance
    jid.first <- sapply(args[1:Nsend], function(x) rcloud.call(function.name, args = x, 
                                                           packages = packages,
                                                           repos = repos,
                                                           globals = globals,
                                                           uid = uid))

    # Sleep to allow sample jobs to finish
    count <- 0
    finished <- FALSE
    while(TRUE) {
      Sys.sleep(5)
      finished <- FALSE
      try(finished <- rcloud.finished(jid.first))
      if(finished) break
      print(count)
      count <- count + 1
      if(count > Twait/5) break
    }

    if(finished) {
      # Calculate the slowdown metric
      # This metric is based off of two factors:
      # 1) t.wait - the overhead of starting each job
      # 2) t.run - the time spent actually running the job
      # These numbers are calculated from each jobs total runtime and the time spent
      # actually running the job (i.e. t.wait = t.total - t.run)

      info <- rcloud.info(jid.first)
      t.total <- sapply(names(info$info), function(x) info$info[[x]]$runtime)

      results <- rcloud.result(jid.first, result.only = FALSE)
      t.run <- sapply(results[2,], c)

      slowdown = sum(t.total)/sum(t.run) # a "perfect" slowdown is 1 (i.e. best case)

      t.total <- mean(t.total)
      t.run <- mean(t.run)

      # n is the bunching factor, i.e. the number of jobs to group together
      # remember that alpha is our target slowdown (the slowdown we hope to achieve
      # by setting the bunching factor to n)
      n <- round(t.total / (t.total - t.run * (slowdown - alpha)))

      jid.next <- c()
      groups <- ceiling((length(args) - Nmin) / n) # number of jobs to submit (after batching)

      for(i in 1:groups) {
        sub.ids <- 1:min(n, length(args) - Nmin - (i-1)*n)

        sub.args <- (Nmin + (i-1)*n + 1):(Nmin + (i-1)*n + sub.ids[length(sub.ids)])
        sub.args <- args[sub.args]

        jid <- rcloud.call(function.name, args = sub.args,
                           packages = packages,
                           repos = repos,
                           globals = globals,
                           uid = uid,
                           n = length(sub.ids))
        jid.next = c(jid.next, as.numeric(paste(jid, ".", sub.ids, sep="")))
      }
      return(c(jid.first, jid.next))
    # We gave up waiting for info on the status of our jobs. Just send them all
    # as seperate jobs
    } else {
      jid.next <- sapply(args[1:5], function(x) rcloud.call(function.name, args = x, 
                                                            packages = packages,
                                                            repos = repos,
                                                            globals = globals,
                                                            uid = uid))
      return(c(jid.first, jid.next))
    }
  }
}

#' Retries the status of job(s) from the PiCloud server
#'
#' @param jids One or more job ids 
rcloud.info <- function(jids)
{
  result <- rcloud.rest.info(floor(jids))

  if(length(jids) == 1) return(result$info)

  result
}

#' Returns true if all the jobs in jids are finished
#' 
#' @param jids One or more job ids
rcloud.finished <- function(jids)
{
  result <- rcloud.rest.info(floor(jids))
  
  for(jid in names(result$info)) {
    if(result$info[[jid]]$status != "done") return(FALSE)
  }
  TRUE
}

#' Retrieves the result of jobs from the PiCloud server
#'
#' @param jids One or more PiCloud job ids
rcloud.result <- function(jids, result.only = TRUE)
{
  if(length(jids) > 1) {
    sapply(jids, function(jid) rcloud.result(jid, result.only))
  } else {
    if(result.only) {
      result <- rcloud.rest.result(floor(jids))$result
      ids <- as.numeric(unlist(strsplit(as.character(jids), "\\.")))

      if(length(ids) == 1 || ids[2] == 0) return(result)
      return(unlist(result[ids[2]]))
    } else {
      rcloud.rest.result(floor(jids))
    }
  }
}
