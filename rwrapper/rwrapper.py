import cloud
import rpy2.robjects as robjects

def wrapper(r_filename, f, arg, packages, globals):
  rscript = cloud.files.getf(r_filename)
  robjects.r(rscript.read()) 
  rf = robjects.r['Rfunct']
  return rf(f, arg, packages, globals)[0]
