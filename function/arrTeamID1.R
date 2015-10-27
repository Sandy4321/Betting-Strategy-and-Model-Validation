arrTeamID <- function(mbase, spboData, parallel=TRUE){
  suppressPackageStartupMessages(require('stringdist', quietly=TRUE))
  suppressPackageStartupMessages(require('doParallel', quietly=TRUE))
  suppressPackageStartupMessages(require('plyr', quietly=TRUE))
  suppressPackageStartupMessages(require('dplyr', quietly=TRUE))
  suppressPackageStartupMessages(require('sqldf', quietly=TRUE))
  
  if((!is.list(mbase)) & (names(mbase)!= c('datasets','others','corners')) & (!is.data.frame(spboData))){
    stop('Please apply readfirmDatasets() to get the firm A data and readSPBO2() to get the spboData livescore data !')
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
    
  df <- data.frame(teamID1=c(tmID1,tmID2),spboTeamID=c(spboTeamID[spboTeamID %in% tmID],rep(NA,length(tmID2))),
                   Match=c(rep(TRUE,length(tmID1)),rep(FALSE,length(tmID2))))
  
  vb1 <- data.frame(mbase$datasets)[(tmID2 %in% mbase$datasets$Home)|(tmID2 %in% mbase$datasets$Away),c('DateUK','Home','Away')]
  vb2 <- data.frame(spboData)[(tmp.spbo %in% spboData$Home)|(tmp.spbo %in% spboData$Away),c('DateUK','Home','Away')]
  vb1 <- vb1[unique(as.character(vb1$DateUK)) %in% unique(as.character(vb2$DateUK)),c('DateUK','Home','Away')]
  vb2 <- vb2[unique(as.character(vb2$DateUK)) %in% unique(as.character(vb1$DateUK)),c('DateUK','Home','Away')]
  vb1 <- vb1[!duplicated(vb1),] %>% .[order(.$DateUK,decreasing=FALSE),]
  vb2 <- vb2[!duplicated(vb2),] %>% .[order(.$DateUK,decreasing=FALSE),]
  vb1$DateUK <- factor(vb1$DateUK); vb2$DateUK <- factor(vb2$DateUK)
  
  ## The kick-off time of the matches which the team names does not found exatcly match in spbo team names. 
  #'@ length(unique(vb1$DateUK))
  #[1] 12200
  #'@ nrow(vb1[unique(vb1$DateUK) %in% unique(vb2$DateUK),])
  #[1] 12131
  ## There will be scrapping error, website kick-off time error, time-zone convert error.
  ##   We know that 69 matches missing (Firm A data)
  tmp.tmID <- llply(split(vb1,vb1$DateUK), function(x) {
    y <- c(as.character(x$Home), as.character(x$Away)); y[y %in% tmID2]},.parallel=parallel)
  tmp.spbotmID <- llply(split(vb2,vb2$DateUK), function(x) {
    y <- c(as.character(x$Home), as.character(x$Away)); y[y %in% tmp.spbo]},.parallel=parallel)
  tmp.tmID2 <- tmp.tmID[names(tmp.tmID) %in% names(tmp.spbotmID)]
  tmp.spbotmID2 <- tmp.spbotmID[names(tmp.tmID) %in% names(tmp.spbotmID)]
  
  nw <- llply(tmp.tmID2,seq); tmp.spbo2 <- llply(tmp.tmID2,seq)
  llply(as.list(seq(tmp.tmID2)), function(i) {
    llply(as.list(seq(tmp.tmID2[[i]])), function(j){
      nw[names(nw)[i]][j] <- min(stringdist(tmp.tmID2[names(nw)[i]][j], tmp.spbotmID2[names(tmp.spbo2)[i]], method='dl'))
      if(nw[[i]][j]==0){
        tmp.spbo2[names(tmp.spbo2)[i]][j] <- tmp.spbotmID2[names(tmp.spbo2)[i]]
      }else{
        tmp.spbo2[names(tmp.spbo2)[i]][j] <- tmp.spbotmID2[amatch(tmp.tmID2[names(nw)[i]][j],
                                             tmp.spbotmID2[names(tmp.spbo2)[i]], method='dl',maxDist=nw[names(nw)[i]][j])]
      }
      tmp.spbo2[names(tmp.spbo2)[i]][j]
    },.parallel=parallel)
  },.parallel=parallel)
  
  ## 2) Apply stringdist regular expression to match the most approximate matching team names
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
    
  stringdistList <- function(mthed, tmp.tmID, tmp.spbo, parallel=parallel) {
    if(!is.vector(mthed)){stop('Please enter a vector of stringdist method!')}
    ## > length(tmp.spbotmID[names(tmp.spbotmID) %in% names(tmp.tmID)])
    ## [1] 1861
    ## > length(tmp.tmID[names(tmp.tmID) %in% names(tmp.spbotmID)])
    ## [1] 1861
    
    strList <- llply(as.list(mthed),function(x){
      as.character(na.omit(unlist(llply(as.list(seq(tmp.tmID)),function(i) {
        nw <- tmp.spbo2 <- vector()
        nw[i] <- min(stringdist(tmp.tmID[i], tmp.spbotmID, method=x))
        
        if(nw[i]==0){
          tmp.spbo2[i] <- tmp.spbotmID[i]
        }else{
          tmp.spbo2[i] <- tmp.spbotmID[amatch(tmp.tmID[i], tmp.spbotmID, method=x, maxDist=nw[i])]
        }
        tmp.spbo2
    },.parallel=parallel))))},.parallel=parallel)
    strList <- data.frame(strList)
    names(strList) <- mthed
    return(strList)}
  ## ---------------------------------------------------------------------------------
  mthed <- c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw','soundex')
  sub.spboTeamID <- stringdistList(mthed=mthed,parallel=parallel)
    
  new.col <- data.frame(rep(list(as.character(df$teamID1[df$Match==TRUE])),length(mthed)))
  names(new.col) <- mthed
  df2 <- rbind(new.col,sub.spboTeamID)
  dfm <- tbl_df(cbind(df,df2))
  rm(tmID1,tmID2,tmp.spbo,new.col,df,df2)
    
  return(dfm)
}


