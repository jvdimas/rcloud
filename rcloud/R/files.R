# TODO: Error Handling

#' Put a file to your PiCloud S3 bucket
#' Requires that PiCloud api key has been set (rcloud.setkey)
#' 
#' @param local.name Local filename to upload. Can also be a list in which case
#'        if remote.name is not null it should have the same length as local.name
#' @param remote.name Remote name for the file. If null uses local.name
rcloud.files.put <- function(local.name, remote.name = NULL)
{
  if(length(local.name) > 1) {
    if(!is.null(remote.name)) stopifnot(length(local.name) == length(remote.name))

    return(sapply(1:length(local.name), 
                  function(i) rcloud.files.put(local.name[i], remote.name[i])))
  }

  if(is.null(remote.name)) remote.name <- local.name

  # API response is an AWS ticket that needs to be posted to
  ticket <- rcloud.rest.files.put(remote.name)

  request.url <- ticket$params$action
  params <- ticket$ticket

  # AWS requires that the arguments to this request be in a specific order
  params[[length(params) + 1]] <- fileUpload(local.name)
  names(params)[length(params)] <- "file"

  curl.opts <- list(followlocation = TRUE)
  
  # TODO: What we POST is binary, but response is text. Currently we're interpreting
  # the response as binary as well
  response <- postForm(request.url, .params = params, .opts = curl.opts)

  # TODO: return true/false for success, failure
}

#' Get a file from the user's PiCloud S3 bucket
#' Requires that PiCloud api key has been set (rcloud.setkey)
#'
#' @param remote.name Remote filename to be downloaded or a list of files to download
#' @param local.name Optional path to download the file to. Will default to remote.name
#'        in the current working directory.
#' @param write.file If true writes file to disk. Otherwise returns the contents of the 
#'        file
#' @return If write.file is true returns true for success. Otherwise returns the contents
#'         of the file (or false on failure)
rcloud.files.get <- function(remote.name, local.name = NULL, write.file = TRUE)
{
  if(length(remote.name) > 1) {
    if(!is.null(local.name)) stopifnot(length(local.name) == length(remote.name))   

    return(sapply(1:length(remote.name),
                  function(i) rcloud.files.get(remote.name[i], local.name[i], write.file)))
  }

  if(is.null(local.name)) local.name <- remote.name

  # API response is a ticket where we find information to actually download the file
  ticket <- rcloud.rest.files.get(remote.name)
  
  # Primitve error checking for now
  if(names(ticket)[1] == "error") {
    warning(ticket$error$msg)
    return(FALSE)
  }

  request.url <- ticket$params$action
  header <- ticket$ticket # additional header arguments specified in ticket

  file.contents <- getURL(request.url, httpheader = header)

  if(write.file) {
    write(file.contents, file = local.name)
    return(TRUE)
  } else return(file.contents)
}

#' Deletes a file or files from the user's PiCloud S3 bucket
#' Requires that PiCloud api key has been set (rcloud.setkey)
#' 
#' @param remote.name Remote name or names to be deleted
#' @return True on success
rcloud.files.delete <- function(remote.name)
{
  if(length(remote.name) > 1) return(sapply(remote.name, rcloud.files.delete))

  response <- rcloud.rest.files.delete(remote.name)

  if(!is.null(response$deleted)) return(TRUE)
  else return(response)
}

#' List all files currently stored on PiCloud S3 bucket
#' Requires that PiCloud api key has been set (rcloud.setkey)
#' 
#' @return List of files currently in bucket.
rcloud.files.list <- function()
{
  response <- rcloud.rest.files.list()

  response$files
}

#' Returns true if remote.name exists on user's PiCloud S3 bucket
#' Requries that the PiCloud api key has been set (rcloud.setkey)
#'
#' @param remote.name Name of file to check or optionally a list of files
#' @param size If true also returns file size
rcloud.files.exists <- function(remote.name, size = FALSE)
{
  if(length(remote.name) > 1) 
    return(sapply(remote.name, function(x) rcloud.files.exists(x, size = size)))

  response <- rcloud.rest.files.exists(remote.name)

  if(size) return(response)
  else return(response$exists)
}
