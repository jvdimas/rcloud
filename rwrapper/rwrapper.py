import cloud
import rpy2.robjects as robjects

def rwrapper(r_filename, f, arg, packages, globals, n):
  # Get neccessary files to expose rcloud functions to calling code
  # cloud.files.get("files.R", "/tmp/files.R")
  # cloud.files.get("rest.R", "/tmp/rest.R")
  # cloud.files.get("rcloud.R", "/tmp/rcloud.R")

  rscript = cloud.files.getf(r_filename)
  robjects.r(rscript.read()) 
  rf = robjects.r['Rfunct']
  return rf(f, arg, packages, globals, n)[0]
