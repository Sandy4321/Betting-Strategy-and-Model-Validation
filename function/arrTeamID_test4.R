arrTeamID <- function(mbase, spboData, match.by='PartialMatch', stringdist.method=c('osa','lv','dl','hamming',
                                                                                    'lcs','qgram','cosine','jaccard','jw','soundex'), levDist=0.1, parallel=FALSE){
  ## Please choose eihther match.by='PartialMatch' or match.by='stringdistList' which is agrep() or 
  ##   stringdist().
  
  if((!is.list(mbase)) & (names(mbase)!= c('datasets','others','corners')) & (!is.data.frame(spboData))){
    stop('Please apply readf1irmDatasets() to get the firm A data and readSPBO2() to get the spboData 
         livescore data !')
  }
  if((match.by!='PartialMatch')&(match.by!='stringdistList')){
    stop('Please choose either match.by="PartialMatch" or match.by="stringdistList" as your approximate 
         matching method !')
  }
  
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
  
  ## Loading packages
  suppressPackageStartupMessages(require('plyr', quietly=TRUE))
  suppressPackageStartupMessages(require('dplyr', quietly=TRUE))
  suppressPackageStartupMessages(require('tidyr', quietly=TRUE))
  
  teamID <- sort(unique(c(as.character(mbase$datasets$Home), as.character(mbase$datasets$Away))))
  spboTeamID <- sort(unique(c(unique(as.vector(spboData$Home)), unique(as.vector(spboData$Away)))))
  
  ## Filter and drop the first-half, corners and other games
  tmID <- teamID[!teamID %in% mbase$others]
  
  ## 1) Filter the exactly match team names with spboTeamID,
  ##      seperate it to ease the processing time to find the approximate team name in section (2).
  ## (a1) Duplicated team names
  tmID1A <- tmID[tmID %in% spboTeamID]
  spboTM1A <- spboTeamID[spboTeamID %in% tmID]
  
  ## (a2) Not duplicated team names
  tmID1B <- tmID[!tmID %in% spboTeamID]
  spboTM1B <- spboTeamID[!spboTeamID %in% tmID]
  
  ## (b1) Partial match team names, only capital letter difference
  tmID2A <- tmID1B[tolower(tmID1B) %in% tolower(spboTM1B)]
  spboTM2A <- spboTM1B[tolower(spboTM1B) %in% tolower(tmID1B)]
  
  ## (b2) Partial match team names, not only capital letter difference
  tmID2B <- tmID1B[!tmID1B %in% tmID2A]
  spboTM2B <- spboTM1B[!spboTM1B %in% spboTM2A]
  
  ## 2) Apply regular expression to seperate the women team, B team or U21 etc since it will be the most approximate team names.
  ##   For example: 
  ## > tmID[grep('1860',tmID)]
  ## [1] "1860 Munchen"
  ## > spboTeamID[grep('Munchen',spboTeamID)]
  ## [1] "1860 Munchen II"          "1860 Munchen U17"        
  ## [3] "1860 Munchen U19"         "Bayern Munchen"          
  ## [5] "Bayern Munchen BA"        "Bayern Munchen II"       
  ## [7] "Bayern Munchen U17"       "Bayern Munchen U19"      
  ## [9] "Bayern Munchen Women"     "FFC Wacker Munchen Women"
  ## [11] "TSV 1860 Munchen"
  ##
  ## "1860 Munchen II" will be matched after I tried to apply all 10 methods inside stringdist() and also
  ## tried the while loops Maximum Likelihood which refer from below website :
  ## http://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets
  ## since second team has only " II" 3 different characters but "TSV " has 4 different characters.
  ## However the correct team name is "TSV 1860 Munchen" since II is the second team. Therefore I need to
  ## separate the second/third team, U18/U19/U23 team, reserves team etc.
  ##
  ## (a1) Separate women team names
  wm <- as.list(c(' women',' woman',' female',' (w)',' (wm)'))
  tmID3A <- unlist(llply(wm,function(x) tmID2B[grep(tolower(x),tolower(tmID2B),fixed=TRUE)]))
  spboTM3A <- unlist(llply(wm,function(x) spboTM2B[grep(tolower(x),tolower(spboTM2B),fixed=TRUE)]))
  
  ## (a2) Non-women team names
  tmID3B <- tmID2B[!tmID2B %in% tmID3A]
  spboTM3B <- spboTM2B[!spboTM2B %in% spboTM3A]
  rm(wm)
  
  ## (b1) Separate second team, reserved team or U18/U21 etc team names based on women/men
  ext.lst <- c(list(c(' 2',' 02',' B',' II'),c(' 3',' 03',' C',' III'),c(' 4',' 04',' D',' IV'),
                    c(' 5',' 05',' E',' V'),c(' 6',' 06',' F',' VI')), paste0(' U',seq(15,23)),
               list(c(' Res',' Rev',' Reserve',' Reserved',' Reserves')),' Youth',' All Stars',' Beach Soccer')
  
  tmID4A <- unlist(llply(as.list(unlist(ext.lst2)),function(x){
            c(tmID3A,tmID3B)[grep(tolower(x), tolower(c(tmID3A,tmID3B)))]},.parallel=parallel))
  
  spboTM4A <- unlist(llply(as.list(unlist(ext.lst2)),function(x){
            c(spboTM3A,spboTM3B)[grep(tolower(x), tolower(c(spboTM3A,spboTM3B)))]},.parallel=parallel))
  
  
  ## --------------------------------------------------------------------------------------------------
  ## test 01
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  llply(list(tmID3A,tmID3B),function(x){
    llply(seq(ext.lst), function(i) {
      llply(seq(ext.lst[[i]]),function(j){
        x[(tolower(substrRight(x,7)) %in% tolower(ext.lst[[i]][j]))|
            (tolower(substrRight(x,3)) %in% tolower(ext.lst[[i]][j]))|
            (tolower(substrRight(x,2)) %in% tolower(ext.lst[[i]][j]))|
            (tolower(substrRight(x,1)) %in% tolower(ext.lst[[i]][j]))]},.parallel=parallel)},.parallel=parallel)},.parallel=parallel)
  
  ## test 01
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  llply(list(tmID3A,tmID3B),function(x){
    llply(seq(ext.lst), function(i) {
      llply(seq(ext.lst[[i]]),function(j){
        x[(tolower(substrRight(x,7)) %in% tolower(ext.lst[[i]][j]))|
            (tolower(substrRight(x,3)) %in% tolower(ext.lst[[i]][j]))|
            (tolower(substrRight(x,2)) %in% tolower(ext.lst[[i]][j]))|
            (tolower(substrRight(x,1)) %in% tolower(ext.lst[[i]][j]))]},.parallel=parallel)},
      .parallel=parallel)},.parallel=parallel)
  
  
  ## test 02
  ext.lst2 <- c(list(c('\\s{1,}2\\s{1,}|\\s{1,}2$','\\s{1,}02\\s{1,}|\\s{1,}02$','\\s{1,}B\\s{1,}|\\s{1,}B$',
              '\\s{1,}II\\s{1,}|\\s{1,}II$'),c('\\s{1,}3\\s{1,}|\\s{1,}3$','\\s{1,}03\\s{1,}|\\s{1,}03$',
              '\\s{1,}C\\s{1,}|\\s{1,}C$','\\s{1,}III\\s{1,}|\\s{1,}III$'),c('\\s{1,}4\\s{1,}|\\s{1,}4$',
              '\\s{1,}04\\s{1,}|\\s{1,}04$','\\s{1,}D\\s{1,}|\\s{1,}D$','\\s{1,}IV\\s{1,}|\\s{1,}IV$'),
              paste0('\\sU',seq(15,23),'\\s|\\s',seq(15,23),'$'),
              list(c('\\sRes\\s|\\sRes$','\\sRev\\s|\\sRev$','\\sReserve\\s|\\sReserve$','\\sReserved\\s|\\sReserved$',
              '\\sReserves\\s|\\sReserves$')),'\\sYouth\\s|\\sYouth$','\\sAll\\s{1}Stars\\s|\\sAll\\s{1}Stars$',
                     '\\sBeach\\s{1}Soccer\\s|\\sBeach\\s{1}Soccer$'))
  
  tmID4A <- llply(list(tmID3A,tmID3B),function(x){
    llply(seq(ext.lst2), function(i) {
      unlist(llply(seq(ext.lst2[[i]]),function(j){
        x[grep(tolower(ext.lst[[i]][j]), tolower(x))]},.parallel=parallel))},.parallel=parallel)},.parallel=parallel)
  
  spboTM4A <- llply(list(spboTM3A,spboTM3B),function(x){
    llply(seq(ext.lst2), function(i) {
      unlist(llply(seq(ext.lst2[[i]]),function(j){
        x[grep(tolower(ext.lst[[i]][j]), tolower(x))]},.parallel=parallel))},.parallel=parallel)},.parallel=parallel)
  
  ## --------------------------------------------------------------------------------------------------
  tmID4A <- llply(list(tmID3A,tmID3B),function(x){
    llply(ext.lst, function(y) {
      x[(tolower(substr(x,nchar(x)-7,nchar(x))) %in% tolower(y))|
          (tolower(substr(x,nchar(x)-3,nchar(x))) %in% tolower(y))|
          (tolower(substr(x,nchar(x)-2,nchar(x))) %in% tolower(y))|
          (tolower(substr(x,nchar(x)-1,nchar(x))) %in% tolower(y))]},.parallel=parallel)},.parallel=parallel)
  ## substrRight()
  ## http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  
  spboTM4A <- llply(list(spboTM3A,spboTM3B),function(x){
    llply(ext.lst, function(y) {
      x[(tolower(substr(x,nchar(x)-7,nchar(x))) %in% tolower(y))|
          (tolower(substr(x,nchar(x)-3,nchar(x))) %in% tolower(y))|
          (tolower(substr(x,nchar(x)-2,nchar(x))) %in% tolower(y))|
          (tolower(substr(x,nchar(x)-1,nchar(x))) %in% tolower(y))]},.parallel=parallel)},.parallel=parallel)
  names(tmID4A) <- names(spboTM4A) <- c('women','men')
  rm(ext.lst)
  
  ## (b2) First team seperated by men/women
  tmID4B <- llply(list(tmID3A,tmID3B),function(x) x[!x %in% unlist(tmID4A)],.parallel=parallel)
  spboTM4B <- llply(list(spboTM3A,spboTM3B),function(x) x[!x %in% unlist(spboTM4A)],.parallel=parallel)
  
  ## ---------------------------------------------------------------------------------
  ## match.by=='stringdistList'
  if((match.by!='PartialMatch')|(match.by=='stringdistList')){
    ## Load the stringdist
    source(paste0(getwd(),'/function/stringdistList.R'))
    df3A <- stringdistList(method=stringdist.method, tmID_A=tmID3A, tmID_B=spboTM3A, levDist=levDist, parallel=parallel)
    df3B <- stringdistList(method=stringdist.method, tmID_A=tmID3B, tmID_B=spboTM3B, levDist=levDist, parallel=parallel)
  }
  
  ## ---------------------------------------------------------------------------------
  ## match.by=='PartialMatch'
  if((match.by=='PartialMatch')|(match.by!='stringdistList')){  
    df3A <-
  }
  ## ---------------------------------------------------------------------------------
  
  }




















