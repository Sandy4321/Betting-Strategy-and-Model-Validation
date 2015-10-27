arrTeamID <- function(mbase, spboData, parallel=TRUE){
  suppressPackageStartupMessages(require('stringdist', quietly=TRUE))
  suppressPackageStartupMessages(require('doParallel', quietly=TRUE))
  suppressPackageStartupMessages(require('plyr', quietly=TRUE))
  suppressPackageStartupMessages(require('dplyr', quietly=TRUE))
  suppressPackageStartupMessages(require('sqldf1', quietly=TRUE))
  
  if((!is.list(mbase)) & (names(mbase)!= c('datasets','others','corners')) & (!is.data.frame(spboData))){
    stop('Please apply readf1irmDatasets() to get the firm A data and readSPBO2() to get the spboData livescore data !')
  }
  
  if(parallel==TRUE){
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    #'@ cl <- makePSOCKcluster(3)
    doParallel::registerDoParallel(cores = 3)
    #' @BiocParallel::register(MulticoreParam(workers=2))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  }
  
  teamID <- sort(unique(c(as.character(mbase$datasets$Home), as.character(mbase$datasets$Away))))
  spboTeamID <- sort(unique(c(unique(as.vector(spboData$Home)),unique(as.vector(spboData$Away)))))
  
  ## Filter and drop the first-half, corners and other games
  tmID <- teamID[!teamID %in% mbase$others]
  
  ## 1) Filter the exactly match team names with spboTeamID,
  ##      seperate it to ease the processing time to find the approximate team name in section (2).
  tmID1 <- tmID[tmID %in% spboTeamID]
  tmID2 <- tmID[!tmID %in% spboTeamID]
  tmp.spbo <- spboTeamID[!spboTeamID %in% tmID]
  
  df1 <- data.frame(teamID1=c(tmID1,tmID2),spboTeamID=c(spboTeamID[spboTeamID %in% tmID],rep(NA,length(tmID2))),
                   Match=c(rep(TRUE,length(tmID1)),rep(FALSE,length(tmID2))))
  
  ## 2) Apply stringdist() to match the most approximate matching team names
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
  
  stringdistList <- function(mthed, tmp.tmID, tmp.spbotmID, parallel=parallel) {
    if(!is.vector(mthed)&!is.vector(tmp.tmID)&!is.vector(tmp.spbotmID)){
      stop('Please enter 3 vectors which are mthed, tmp.tmID and tmp.spbotmID!')}
    
    strList <- llply(as.list(mthed),function(x){
      res <- llply(as.list(seq(tmp.tmID)),function(i) {
        nw <- tmp.spbo2 <- vector()
        nw[i] <- min(stringdist(tmp.tmID[i], tmp.spbotmID, method=x))
        
        if(nw[i]==0){
          tmp.spbo2[i] <- tmp.spbotmID[i]
        }else{
          tmp.spbo2[i] <- tmp.spbotmID[amatch(tmp.tmID[i], tmp.spbotmID, method=x, maxDist=nw[i])]}
        tmp.spbo2
      },.parallel=parallel)
      as.character(na.omit(unlist(res)))},.parallel=parallel)
    strList <- data.frame(strList)
    names(strList) <- mthed
    return(strList)}
  ## ---------------------------------------------------------------------------------
  mthed <- c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw','soundex')
  sub.spboTeamID <- stringdistList(mthed=mthed, tmp.tmID=tmID2, tmp.spbotmID=tmp.spbo, parallel=parallel)
  
  ## ---------------------------------------------------------------------------------  
  ## Might choose 1 %in% 10 within mthed=mthed[1] since there are only 10 teams name to be further stringdist
  ##  to get the EM (Maximum Likelihhod) team names, and directly replace into the data.frame
  ##  df1[df1$Match==FALSE,'spboTeamID']
  ## Due to 
  spboLst <- llply(split(sub.spboTeamID, seq(nrow(sub.spboTeamID))), function(x) unique(as.vector(unlist(x))),
                   .parallel=parallel)
  res <- sapply(as.list(seq(nrow(sub.spboTeamID))), function(i) {
    spboLst[[i]][amatch(as.list(tmID2)[[i]], spboLst[[i]], method=mthed[1],
                        maxDist=min(stringdist(as.list(tmID2)[[i]], spboLst[[i]], method=mthed[1])))]})
  df1$spboTeamID <- factor(c(na.omit(as.character(df1$spboTeamID)),res))
  
  ## ---------------------------------------------------------------------------------  
  new.col <- data.frame(rep(list(as.character(df1$teamID1[df1$Match==TRUE])),length(mthed)))
  names(new.col) <- mthed
  df2 <- rbind(new.col,sub.spboTeamID)
  dfm <- tbl_df(cbind(df1,df2))
  rm(tmID1,tmID2,tmp.spbo,new.col,spboLst,df1,df2,res)
  
  return(dfm)
}


