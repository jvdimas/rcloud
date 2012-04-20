#' Executes a function remotely on the PiCloud infrastructure
#' Requires that rcloud.setkey has first been called
#'
#' @param function.name The function to execute remotely
#' @param args The arguments to the function as a named list
#' @param uid A unique uid specific to a user's account
rcloud.call <- function(function.name, args = list(), uid)
{
  # TODO: most everything

  # Serialize the arguments in ASCII form. To post to RCurl we must convert to a
  # character vector (it fails for raw vectors) so enforce ASCII to avoid misplaced
  # null characters. Finally place it in a multipart form container so it is treated
  # as a binary argument by PiCloud. Note that the filename field must be set to
  # an empty string or PiCloud will ignore the argument.
  args.serialized <- serialize(args, NULL, ascii = TRUE) 
  args.serialized <- rawToChar(args.serialized)
  args.serialized <- fileUpload("", contents = args.serialized)

  # Serialize the function itself
  # TODO: This needs a lot of additional work (i.e. if the function calls additional
  # functions, packages, global variables, etc)
  f.serialized <- rawToChar(serialize(function.name, NULL, ascii = TRUE))
  f.serialized <- fileUpload("", contents = f.serialized)
    
  result <- rcloud.rest.call("rwrapper", uid, params = 
                             list(r_filename = "\"play2.R\"", 
                                  arg = args.serialized,
                                  f = f.serialized))

  if(names(result)[1] == "error") warning( paste("Server Error Msg: ", result$error$msg) )
  
  result$jid
}

rcloud.map <- function(function.name, args = list(), uid)
{
  sapply(args, function(x) rcloud.call(function.name, x, uid))
}

#' Retries the status of job(s) from the PiCloud server
#'
#' @param jids One or more job ids 
rcloud.info <- function(jids)
{
  result <- rcloud.rest.info(jids)

  if(length(jids) == 1) return(result$info)

  result
}

#' Returns true if all the jobs in jids are finished
#' 
#' @param jids One or more job ids
rcloud.finished <- function(jids)
{
  result <- rcloud.rest.info(jids)
  
  done = TRUE
  for(jid in names(result$info)) {
    if(result$info[[jid]]$status != "done") done = FALSE
  }
  done
}

#' Retrieves the result of jobs from the PiCloud server
#'
#' @param jids One or more PiCloud job ids
rcloud.result <- function(jids)
{
  if(length(jids) > 1) {
    sapply(jids, rcloud.result)
  } else {
    rcloud.rest.result(jids)
  }
}
