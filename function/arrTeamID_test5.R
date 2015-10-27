arrTeamID <- function(mbase, spboData, match.by='partialMatch', stringdist.method=c('osa','lv','dl',
                      'hamming','lcs','qgram','cosine','jaccard','jw','soundex'), levDist=0.1, 
                      parallel=FALSE){
  ## Please choose eihther match.by='partialMatch' or match.by='stringdistList' which is agrep() or 
  ##   stringdist().
  
  if((!is.list(mbase)) & (names(mbase)!= c('datasets','others','corners')) & (!is.data.frame(spboData))){
    stop('Please apply readf1irmDatasets() to get the firm A data and readSPBO2() to get the spboData 
         livescore data !')
  }
  if((match.by!='partialMatch')&(match.by!='stringdistList')){
    stop('Please choose either match.by="partialMatch" or match.by="stringdistList" as your approximate 
         matching method !')
  }
  
  if(parallel==TRUE){
    require('doParallel', quietly=TRUE)
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    #'@ cl <- makePSOCKcluster(8)
    doParallel::registerDoParallel(cores = 16)
    #' @BiocParallel::register(MulticoreParam(workers=8))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  }
  
  ## Loading packages
  require('plyr', quietly=TRUE)
  require('dplyr', quietly=TRUE)
  require('tidyr', quietly=TRUE)
  require('stringr', quietly=TRUE)
  
  teamID <- sort(unique(c(as.character(mbase$datasets$Home), as.character(mbase$datasets$Away))))
  spboTeam <- sort(c(as.vector(spboData$Home), as.vector(spboData$Away))) %>% ifelse(nchar(.)==0,NA,.) %>% na.omit
  spboTeamID <- sort(unique(spboTeam))
  
  ## Filter and drop the first-half, corners and other games
  tmID <- teamID[!teamID %in% mbase$others]
  
  
  ## STEP 1) Filter the exactly match (or different Capital letter teams' name) teams' name with spboTeamID,
  ##    seperate it to ease the processing time to find the approximate teams' name in section (2).
  ## (a1) Duplicated teams' name
  tmID1A <- tmID[tmID %in% spboTeamID]
  spboTM1A <- spboTeamID[spboTeamID %in% tmID]
  
  ## (a2) Not duplicated teams' name
  tmID1B <- tmID[!tmID %in% spboTeamID]
  spboTM1B <- spboTeamID[!spboTeamID %in% tmID]
  
  ## (b1) Partial match teams' name, only capital letter difference
  tmID2A <- tmID1B[tolower(tmID1B) %in% tolower(spboTM1B)]
  spboTM2A <- spboTM1B[tolower(spboTM1B) %in% tolower(tmID1B)]
  
  ## (b2) Partial match teams' name, not only capital letter difference
  tmID2B <- tmID1B[!tmID1B %in% tmID2A]
  spboTM2B <- spboTM1B[!spboTM1B %in% spboTM2A]
  
  
  ## STEP 2) Apply regular expression to filter/seperate the women team, B team or U21 etc prior to partial 
  ##     matching the most approximate teams' name (teams' name others than in STEP 1).
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
  ## (a1) Separate women teams' name
  wm <- as.list(c(' women',' woman',' female',' (w)',' (wm)'))
  tmID3A <- unlist(llply(wm,function(x) tmID2B[grep(tolower(x),tolower(tmID2B),fixed=TRUE)]))
  spboTM3A <- unlist(llply(wm,function(x) spboTM2B[grep(tolower(x),tolower(spboTM2B),fixed=TRUE)]))
  
  ## (a2) Non-women teams' name
  tmID3B <- tmID2B[!tmID2B %in% tmID3A]
  spboTM3B <- spboTM2B[!spboTM2B %in% spboTM3A]
  rm(wm)
  
  ## There has a concern which is second teams' name must be exactly same with first team but only add II, 
  ##  reserved etc to the first team name, for example : Mainz 05 is first team but not fifth reserved team.
  ##  More soccer matches data scrapped will be more accurate, for example if we only scrapped one day data, 
  ##  how can we matching the first team if let say only Chelsea reserved team play on that particular date.
  ##  However there has another concern which is first team 'TSV 1860 Munchen' but second/U19 team termed as 
  ##  '1860 Munchen II', '1860 Munchen U19' etc. Then 'Lincoln' = 'Lincoln City' != 'Lincoln United'
  ##  
  ## Due to I have seperated the youth, reseved, women team, therefore later agrep will also approximately 
  ##  matching the 'TSV 1860 Munchen'.
  ## 
  ## (b1) Separate second team, reserved team or U18/U21 etc teams' name based on women/men
  ext.lst <- as.list(c('2','02','B','II','3','03','C','III','4','04','D','IV',
                    paste0('U',seq(15,23)),'Res','Rev','Reserve','Reserved','Reserves',
                    'Youth','All\\s{1}Stars','Beach\\s{1}Soccer','Futsal'))
  # we can add somemore new element/key words if needed, for example : Futsal, Basketball, Volleybal etc.
  
  wm <- c('\\s{1}women','\\s{1}woman','\\s{1}female','\\s{1}(w)','\\s{1}(wm)')
  
  ext.m <- llply(ext.lst, function(x) paste0('\\s{1}',x,'$'))
  ext.wm <- llply(ext.lst, function(x) {paste(paste0('\\s{1}',x,wm,'\\s{1}|\\s{1}',x,wm,'$|',
            wm,'\\s{1}',x,'\\s{1}|',wm,'\\s{1}',x,'$'),collapse='|')})
  
  ## ---------------------------------------------------------------------------------
  ## Due to not familar with function 'eval(expression(x))'. Here I using substitute function.
  llst <- function(lst=substitute(lst)){
    list('II'=c(lst[[1]],lst[[2]],lst[[3]],lst[[4]]),
         'III'=c(lst[[5]],lst[[6]],lst[[7]],lst[[8]]),
         'IV'=c(lst[[9]],lst[[10]],lst[[11]],lst[[12]]),'U15'=lst[[13]],
         'U16'=lst[[14]],'U17'=lst[[15]],'U18'=lst[[16]],'U19'=lst[[17]],
         'U20'=lst[[18]],'U21'=lst[[19]],'U22'=lst[[20]],'U23'=lst[[21]],
         'Res'=c(lst[[22]],lst[[23]],lst[[24]],lst[[25]],lst[[26]]),
         'Youth'=lst[[27]],'All Stars'=lst[[28]],'Beach Soccer'=lst[[29]],'Futsal'=lst[[30]])
    # we can add somemore new element/key words if needed, for example : Futsal, Basketball, Volleybal etc.
  }
  
  ## ---------------------------------------------------------------------------------
  ## women second/reserved and first team
  tmID4A <- llst(llply(ext.wm,function(x) tmID3A[grep(tolower(x), tolower(tmID3A))],
            .parallel=parallel)) %>% c(.,First=list(tmID3A[!tmID3A %in% unlist(.)]))
  spboTM4A <- llst(llply(ext.wm,function(x) spboTM3A[grep(tolower(x), tolower(spboTM3A))],
              .parallel=parallel)) %>% c(.,First=list(spboTM3A[!spboTM3A %in% unlist(.)]))
  
  ## men second/reserved and first team
  tmID4B <- llst(llply(ext.m,function(x) tmID3B[grep(tolower(x), tolower(tmID3B))],
            .parallel=parallel)) %>% c(.,First=list(tmID3B[!tmID3B %in% unlist(.)]))
  spboTM4B <- llst(llply(ext.m,function(x) spboTM3B[grep(tolower(x), tolower(spboTM3B))],
              .parallel=parallel)) %>% c(.,First=list(spboTM3B[!spboTM3B %in% unlist(.)]))
  rm(ext.lst,wm,ext.m,ext.wm,llst)
  
  ## Second matching on the teams' name key words
  ## (b2) Matching some key words, since there has 'Manchester United' and 'Manchester City' etc
  ext <- c('\\s{1}United','\\s{1}City','\\s{1}FC','\\s{1}BK','\\s{1}Club','\\s{1}SC')
  # we can add somemore new element/key words if needed, for example : AFC, SC, FC.
  
  ext.ky <- c('^AC\\s{1}|\\s{1}AC$',llply(ext, function(x) paste0(x,'\\s{1}|',x,'$')))
  vnames <- c('AC','United','City','FC','BK','Club','SC')
  
  ## women second/reserved team
  tmID5A <- llply(ext.ky,function(x) tmID4A[grep(x,tmID4A)],.parallel=parallel) %>% 
    c(.,rest=list(tmID4A[!tmID4A %in% unlist(.)]))
  spboTM5A <- llply(ext.ky,function(x) spboTM4A[grep(x,spboTM4A)],.parallel=parallel) %>% 
    c(.,rest=list(spboTM4A[!spboTM4A %in% unlist(.)]))
  
  ## men second/reserved team
  tmID5B <- llply(ext.ky,function(x) tmID4B[grep(x,tmID4B)],.parallel=parallel) %>% 
    c(.,rest=list(tmID4B[!tmID4B %in% unlist(.)]))
  spboTM5B <- llply(ext.ky,function(x) spboTM4B[grep(x,spboTM4B)],.parallel=parallel) %>% 
    c(.,rest=list(spboTM4B[!spboTM4B %in% unlist(.)]))
  
  ## (c2) First team seperated by men/women
  ## women first team
  tmID5C <- llply(ext.ky,function(x) tmID4C[grep(x,tmID4C)],.parallel=parallel) %>% 
    c(.,rest=list(tmID4C[!unlist(.) %in% tmID4C]))
  spboTM5C <- llply(ext.ky,function(x) spboTM4D[grep(x,spboTM4D)],.parallel=parallel) %>% 
    c(.,rest=list(spboTM4C[!unlist(.) %in% spboTM4C]))
  
  ## men first team
  tmID5D <- llply(ext.ky,function(x) tmID4D[grep(x,tmID4D)],.parallel=parallel) %>% 
    c(.,rest=list(tmID4D[!unlist(.) %in% tmID4D]))
  spboTM5D <- llply(ext.ky,function(x) spboTM4D[grep(x,spboTM4D)],.parallel=parallel) %>% 
    c(.,rest=list(spboTM4D[!unlist(.) %in% spboTM4D]))
  
  names(tmID5A)[-length(tmID5A)] <- names(tmID5B)[-length(tmID5B)] <- names(tmID5C)[-length(tmID5C)] <- 
    names(tmID5D)[-length(tmID5D)] <- names(spboTM5A)[-length(spboTM5A)] <- 
    names(spboTM5B)[-length(spboTM5B)] <- names(spboTM5C)[-length(spboTM5C)] <- 
    names(spboTM5D)[-length(spboTM5D)] <- vnames
  
  
  
  
  
  
  ## Splot Date by teams' name
  split(spboData$Date,spboData$Home)
  
  sapply(tmID5B, function(x) spboTM5B[agrep(x,spboTM5B,max.distance=0.1)])
  
  ## STEP 3) Option stringdistList() or partialMatch()
  ## (a) match.by=='stringdistList'
  if((match.by!='partialMatch')|(match.by=='stringdistList')){
    ## Load the stringdistList() function
    source(paste0(getwd(),'/function/stringdistList.R'))
    m.fst <- stringdistList(method=stringdist.method, 
                            tmID_A=tmID5B, tmID_B=spboTM5B, levDist=levDist, parallel=parallel)
    wm.fst <- stringdistList(method=stringdist.method, 
                            tmID_A=tmID5A, tmID_B=spboTM5A, levDist=levDist, parallel=parallel)
    m.res <- llply(as.list(seq(tmID4B)), function(i) stringdistList(method=stringdist.method, 
             tmID_A=tmID4B[[i]], tmID_B=spboTM4B[[i]], levDist=levDist, parallel=parallel))
    wm.res <- llply(as.list(seq(tmID4A)), function(i) stringdistList(method=stringdist.method, 
              tmID_A=tmID4A[[i]], tmID_B=spboTM4A[[i]], levDist=levDist, parallel=parallel))
    
    m.fst1 <- stringdistList(method=stringdist.method, 
                             tmID_A=tmID5B, tmID_B=spboTeam[spboTeam %in% spboTM5B], levDist=Inf, parallel=parallel)
    wm.fst1 <- stringdistList(method=stringdist.method, 
                              tmID_A=tmID5A, tmID_B=spboTeam[spboTeam %in% spboTM5A], levDist=Inf, parallel=parallel)
    m.res1 <- llply(as.list(seq(tmID4B)),  function(i) stringdistList(method=stringdist.method, 
                                                                      tmID_A=tmID4B[[i]], tmID_B=spboTeam[spboTeam %in% spboTM4B[[i]]], levDist=Inf, parallel=parallel))
    wm.res1 <- llply(as.list(seq(tmID4A)), function(i) stringdistList(method=stringdist.method, 
                                                                      tmID_A=tmID4A[[i]], tmID_B=spboTeam[spboTeam %in% spboTM4A[[i]]], levDist=Inf, parallel=parallel))
    
  }
  
  ## (b) match.by=='partialMatch'
  if((match.by=='partialMatch')|(match.by!='stringdistList')){  
    ## A rogue character in @coneee's data file borked things for me, so let's do a character code 
    ##  conversion first
    ## Load the partialMatch() function
    source(paste0(getwd(),'/function/partialMatch.R'))
    tmID5A <- iconv(tmID5A)
    tmID5B <- iconv(tmID5B)
    df1 <- partialMatch(tmID5A, spboTM5A)
    df2 <- partialMatch(tmID5B, spboTM5B)
    df1 <- df1 %>% rename(teamID=raw.x, spboTeamID=raw.y, Matching=pass) %>% tbl_df
    df2 <- df2 %>% rename(teamID=raw.x, spboTeamID=raw.y, Matching=pass) %>% tbl_df
    rownames(df1) <- rownames(df2) <- NULL
  }
  ## ---------------------------------------------------------------------------------
  
}