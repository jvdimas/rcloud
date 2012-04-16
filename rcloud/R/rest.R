# Copyright (c) 2012, Jack DiMassimo
# All rights reserved.

# Redistribution and use in source and binary forms, with or without modification, 
# are permitted provided that the following conditions are met:

# 1) Redistributions of source code must retain the above copyright notice, 
# this list of conditions and the following disclaimer.
# 2) Redistributions in binary form must reproduce the above copyright notice, 
# this list of conditions and the following disclaimer in the documentation and/or 
# other materials provided with the distribution.
# 3 ) Neither the name of the <ORGANIZATION> nor the names of its contributors may be 
# used to endorse or promote products derived from this software without specific 
# prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
# IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
# OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY 
# OF SUCH DAMAGE.

require(RCurl)
require(rjson)

# Private function used to make GET requests to the api server
# Returns a string containing the response by the server
.rcloud.rest.get <- function(request, api.url = "https://api.picloud.com/",
                                 key = NULL, secret.key = NULL, 
                                 ssl.verifyhost = FALSE, ssl.verifypeer = FALSE,
                                 timeout = 5)
{
  if(is.null(key)) key <- Sys.getenv("PICLOUD_KEY")
  if(is.null(secret.key)) secret.key <- Sys.getenv("PICLOUD_SECRET_KEY")

  if(is.null(key)) stop("PiCloud access key is not set. See rcloud.setkey")
  if(is.null(secret.key)) stop("PiCloud secret key is not set. See rcloud.setkey")

  userpwd <- paste(key, secret.key, sep=":")
  request.url <- paste(api.url, request, sep="")
  
  response <- getURL(request.url, userpwd = userpwd, ssl.verifyhost = ssl.verifyhost,
                   ssl.verifypeer = ssl.verifypeer, httpauth = 1L, timeout = timeout)
  response
}

# Private function used to make POST requests to the api server
# Returns a string containing the response by the server
# @param params A named list containing the parameters to pass to the server
.rcloud.rest.post <- function(request, params = list(),
                              api.url = "http://api.picloud.com/",
                              key = NULL, secret.key = NULL, 
                              ssl.verifyhost = FALSE, ssl.verifypeer = FALSE,
                              timeout = 5)
{
  if(is.null(key)) key <- Sys.getenv("PICLOUD_KEY")
  if(is.null(secret.key)) secret.key <- Sys.getenv("PICLOUD_SECRET_KEY")

  if(is.null(key)) stop("PiCloud access key is not set. See rcloud.setkey")
  if(is.null(secret.key)) stop("PiCloud secret key is not set. See rcloud.setkey")

  userpwd <- paste(key, secret.key, sep=":")
  request.url <- paste(api.url, request, sep="")
  
  curl.opts <- list(ssl.verifypeer = ssl.verifypeer, ssl.verifyhost = ssl.verifyhost,
                    timeout = timeout, httpauth = 1L, userpwd = userpwd)
  response <- postForm(request.url, .params = params, .opts = curl.opts)
  response
}

rcloud.setkey <- function(key, secret.key)
{
  Sys.setenv(PICLOUD_KEY=key, PICLOUD_SECRET_KEY=secret.key)
}

rcloud.rest.call <- function(function.name, uid, params = list(), key = NULL, 
                             secret.key = NULL,
                             ssl.verifypeer = FALSE, ssl.verifyhost = FALSE, timeout = 5,
                             api.url = "https://api.picloud.com/") 
{
  request <- paste("r", uid, function.name, "", sep="/")
  
  response <- .rcloud.rest.post(request, params = params, key = key,
                                secret.key = secret.key,
                                ssl.verifyhost = ssl.verifyhost,
                                ssl.verifypeer = ssl.verifypeer, 
                                timeout = timeout)
  fromJSON(response) 
}

rcloud.rest.funcinfo <- function(function.name, uid, key = NULL, secret.key = NULL,
                                 ssl.verifypeer = FALSE, ssl.verifyhost = FALSE,
                                 timeout = 5)
{
  request <- paste("r", uid, function.name, "", sep="/")

  result <- .rcloud.rest.get(request, key = key, secret.key = secret.key,
                                 ssl.verifyhost = ssl.verifyhost,
                                 ssl.verifypeer = ssl.verifypeer,
                                 timeout = timeout)
  fromJSON(result)
}

rcloud.rest.info <- function(jids, key = NULL, secret.key = NULL, 
                             ssl.verifypeer = FALSE, ssl.verifyhost = FALSE,
                             timeout = 5)
{
  # Format of request: "job/?jids="
  request <- "job/?jids="
  jids.str <- paste(jids, collapse=",")
  request <- paste(request, jids.str, sep="")

  result <- .rcloud.rest.get(request, key = key, secret.key = secret.key, 
                       ssl.verifyhost = ssl.verifyhost, ssl.verifypeer = ssl.verifypeer,
                       timeout = timeout)
  fromJSON(result)
}

rcloud.rest.kill <- function(jids = NULL, key = NULL, secret.key = NULL,
                             ssl.verifypeer = FALSE, ssl.verifyhost = FALSE,
                             timeout = 5)
{
  request <- "job/kill/"
  if(!is.null(jids)) {
    jids.str <- paste(jids, collapse=",")
    request <- paste(request, "?jids=", jids.str, sep="")
  } else {
    warning("No jids specified, killing all active jobs.")
  }

  .rcloud.rest.get(request, key = key, secret.key = secret.key,
                       ssl.verifyhost = ssl.verifyhost, ssl.verifypeer = ssl.verifypeer,
                       timeout = timeout)
  return() # no return information
}

rcloud.rest.delete <- function(jids, key = NULL, secret.key = NULL,
                               ssl.verifypeer = FALSE, ssl.verifyhost = FALSE,
                               timeout = 5)
{
  request <- "job/delete/?jids="
  jids.str <- paste(jids, collapse=",")
  request <- paste(request, jids.str, sep="")

  .rcloud.rest.get(request, key = key, secret.key = secret.key,
                       ssl.verifyhost = ssl.verifyhost, ssl.verifypeer = ssl.verifypeer,
                       timeout = timeout)
}

rcloud.rest.result <- function(jid, key = NULL, secret.key = NULL,
                              ssl.verifypeer = FALSE, ssl.verifyhost = FALSE,
                              timeout = 5)
{
  request <- paste("job/result/?jid=", jid, sep="")

  result <- .rcloud.rest.get(request, key = key, secret.key = secret.key,
                                 ssl.verifyhost = ssl.verifyhost, 
                                 ssl.verifypeer = ssl.verifypeer,
                                 timeout = timeout)
  fromJSON(result)
}

rcloud.rest.file.get <- function(filename, key = NULL, secret.key = NULL,
                                 ssl.verifypeer = FALSE, ssl.verifyhost = FALSE,
                                 timeout = 5)
{
  request <- paste("file/get/")
  params <- list(name = filename)

  response <- .rcloud.rest.post(request, params = params, key = key,
                                secret.key = secret.key,
                                ssl.verifyhost = ssl.verifyhost,
                                ssl.verifypeer = ssl.verifypeer,
                                timeout = timeout)
  fromJSON(response)
}

rcloud.rest.file.put <- function(filename, key = NULL, secret.key = NULL,
                                 ssl.verifypeer = FALSE, ssl.verifyhost = FALSE,
                                 timeout = 5)
{
  request <- paste("file/new/")
  params <- list(name = filename)

  response <- .rcloud.rest.post(request, params = params, key = key,
                                secret.key = secret.key,
                                ssl.verifyhost = ssl.verifyhost,
                                ssl.verifypeer = ssl.verifypeer,
                                timeout = timeout)
  fromJSON(response)
}

rcloud.rest.file.list <- function(key = NULL, secret.key = NULL,
                                  ssl.verifyhost = FALSE,
                                  ssl.verifypeer = FALSE,
                                  timeout = 5)
{
  request <- paste("file/list/")
  
  response <- .rcloud.rest.get(request, key = key, secret.key = secret.key,
                               ssl.verifyhost = ssl.verifyhost,
                               ssl.verifypeer = ssl.verifypeer,
                               timeout = timeout)
  fromJSON(response)
}

rcloud.rest.file.exists <- function(filename, key = NULL, secret.key = NULL,
                                    ssl.verifyhost = FALSE,
                                    ssl.verifypeer = FALSE,
                                    timeout = 5)
{
  request <- paste("file/exists/")
  params <- list(name = filename)
  
  response <- .rcloud.rest.post(request, params = params, key = key,
                                secret.key = secret.key,
                                ssl.verifyhost = ssl.verifyhost,
                                ssl.verifypeer = ssl.verifypeer,
                                timeout = timeout)
  fromJSON(response)
}

rcloud.rest.file.delete <- function(filename, key = NULL, secret.key = NULL,
                                    ssl.verifyhost = FALSE,
                                    ssl.verifypeer = FALSE,
                                    timeout = 5)
{
  request <- paste("file/delete/")
  params <- list(name = filename)

  response <- .rcloud.rest.post(request, params = params, key = key,
                                secret.key = secret.key,
                                ssl.verifyhost = ssl.verifyhost,
                                ssl.verifypeer = ssl.verifypeer,
                                timeout = timeout)
  fromJSON(response)
}
