Rfunct <- function(f.binary, args.binary, packages.binary, globals.binary)
{
  # Unserialize the arguments
  args <- unserialize(charToRaw(args.binary))

  # Unserialize the function
  f <- unserialize(charToRaw(f.binary))

  # Unserialize the packages
  packages.list <- unserialize(charToRaw(packages.binary))
  packages <- packages.list$names
  repos <- packages.list$repos
  # Try to load the package. If it's not installed install it
  sapply(packages, function(p) 
         tryCatch(require(p, character.only = TRUE),
                  error=install.packages(p, 
                                         repos=repos,
                                         lib="~/R/x86_64-pc-linux-gnu-library/2.12"),
                  finally=require(p, character.only = TRUE)))

  # Unserialize the global variables and attach them
  globals <- unserialize(charToRaw(globals.binary))
  attach(globals)

  # Call the function and record the time it spends executing
  elapsed <- system.time(
  if(is.list(args)) {
    result <- do.call(f, args)
  } else {
    result <- f(args)
  })[3]

  # Include exeuction time with result
  result <- list(result = result, elapsed.time = elapsed)

  # Serialize the result and return it
  result.serialized <- serialize(result, NULL, ascii = TRUE)
  rawToChar(result.serialized)
}
