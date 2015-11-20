stringdistList <- function(method=c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw','soundex'), 
                           tmID_A, tmID_B, parallel=FALSE) {
  ## levDist=Inf, levDist=0.1 stringdist() depreciated and auto turned to maxDist=Inf, here I omit to set levDist
  ## 
  ## Apply stringdist() to match the most approximate matching team names
  ##    Due to if the value of nw[i]=0 exactly match will automatically skip and match the 1 difference
  ##      Hereby seperate to 'exactly matching' and 'approximate matching'
  ## 
  ## ---------------------------------------------------------------------------------
  ## 01. osa - Optimal string aligment, (restricted Damerau-Levenshtein distance).
  ## 02. lv - Levenshtein distance (as in R’s native adist).
  ## 03. dl - Full Damerau-Levenshtein distance.
  ## 04. hamming - Hamming distance (a and b must have same nr of characters).
  ## 05. lcs - Longest common substring distance.
  ## 06. qgram - q-gram distance.
  ## 07. cosine - cosine distance between q-gram profiles
  ## 08. jaccard - Jaccard distance between q-gram profiles
  ## 09. jw - Jaro, or Jaro-Winker distance.
  ## 10. soundex - Distance based on soundex encoding (see below)
  
  ## Setting to omitt all warnings
  options(warn=-1)
  
  suppressPackageStartupMessages(require('stringdist', quietly=TRUE))
  suppressPackageStartupMessages(require('plyr', quietly=TRUE))
  suppressPackageStartupMessages(require('dplyr', quietly=TRUE))
  suppressPackageStartupMessages(require('tidyr', quietly=TRUE))
  
  methd <- c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw','soundex')
  if(!all(method %in% methd)){
    stop('Please select one or more stringdist.method from above:',cat(methd))
  }
  
  if(!is.vector(method)&!is.vector(tmID_A)&!is.vector(tmID_B)){
    stop('Please enter 3 vectors which are method, tmID_A and tmID_B!')}
  
  if(parallel==TRUE){
    suppressPackageStartupMessages(require('doParallel', quietly=TRUE))
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    #'@ cl <- makePSOCKcluster(3)
    doParallel::registerDoParallel(cores = 3)
    #' @BiocParallel::register(MulticoreParam(workers=2))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  }
  
  source(paste0(getwd(),'/function/signature.R'))
  ## Apply signature() which will re-arrange the strings for increased the accuracy of Levenstein or 
  ##   or string distance calculation.
  names(tmID_A) <- sapply(tmID_A, signature)
  names(tmID_B) <- sapply(tmID_B, signature)
  
  ## http://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets
  ## Refer the above link to maximized the likelihood of approximate matching by while loop. However the 
  ##   maxDist has been depreciated. Therefore more data will generate a more accurate matching result. 
  ## 
  dm <- expand.grid(tmID_A,tmID_B) # Distance matrix in long form
  names(dm) <- c('teamID','spboID')
  dm$dist <- stringdist(names(dm$teamID),names(dm$spboID), method=method)
  ## String edit distance (use your favorite function here)
  
  ## ---------------------------------------------------------------------------------------------
  ## Greedy assignment heuristic (Your favorite heuristic here)
  greedyAssign <- function(a,b,dm){
    x <- numeric(length(names(a))) # assign variable: 0 for unassigned but assignable, 
    # 1 for already assigned, -1 for unassigned and unassignable
    while(any(x==0)){
      min_dm <- min(dm[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
      a_sel <- a[dm==min_dm & x==0][1]
      b_sel <- b[dm==min_dm & a == a_sel & x==0][1] 
      x[a==a_sel & b == b_sel] <- 1
      x[x==0 & (a==a_sel|b==b_sel)] <- -1
    }
    cbind(a=a[x==1],b=b[x==1],dm=dm[x==1])
  }
  
  ## ---------------------------------------------------------------------------------------------
  strList <- llply(as.list(method),function(x){
    res <- data.frame(greedyAssign(as.character(dm$teamID),as.character(dm$spboID),dm$dist)) %>% tbl_df
    names(res) <- c('teamID',x,paste0('dist.',x))
    return(res)
    },.parallel=parallel)
  
  strList <- Reduce(function(x, y) merge(x, y, by='teamID'), strList) %>% tbl_df
  return(strList)}