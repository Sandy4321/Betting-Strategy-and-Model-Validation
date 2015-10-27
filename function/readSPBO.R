readSPBO <- function(dateID=dateID, path=path, parallel=FALSE){
  ## Setting to omitt all warnings since there are alot of data validation for manipulation
  options(warn=-1)
  
  ## Loading the packages
  if(!suppressWarnings(require('BBmisc',quietly=TRUE))){
    suppressWarnings(install.packages('BBmisc'))
    suppressAll(require('BBmisc',quietly=TRUE))}
  
  ## Might load RSeleniumUtilities package if needed
  ##  https://github.com/greenore/RSeleniumUtilities
  suppressPackageStartupMessages(library('BBmisc'))
  pkgs <- c('plyr','dplyr','stringr','stringi','doParallel')
  suppressPackageStartupMessages(lib(pkgs)); rm(pkgs)
  
  ## Set parallel computing
  #'@ cl <- makePSOCKcluster(3)
  registerDoParallel(cores=3)
  ## http://www.inside-r.org/r-doc/parallel/detectCores
  #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  
  #' @dateID2 <- llply(split(dateID,substr(dateID,1,4)),as.character)
  spboData <- rbind_all(llply(dateID,function(x){ data.frame(read.csv(file=paste0(getwd(),'/datasets/',path,'/',x,'.csv'),
                                                 header=TRUE,sep=','))},.parallel=parallel))
  return(spboData)
}


