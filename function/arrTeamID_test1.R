arrTeamID <- function(mbase, spboData, parallel=TRUE){
  suppressPackageStartupMessages(require('stringdist', quietly=TRUE))
  suppressPackageStartupMessages(require('doParallel', quietly=TRUE))
  suppressPackageStartupMessages(require('plyr', quietly=TRUE))
  suppressPackageStartupMessages(require('dplyr', quietly=TRUE))
  suppressPackageStartupMessages(require('sqldf', quietly=TRUE))
  suppressPackageStartupMessages(require('tidyr', quietly=TRUE))
  
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
  ## (a1) Duplicated team names
  tmID1A <- tmID[tmID %in% spboTeamID]
  tmp.spbo1A <- spboTeamID[spboTeamID %in% tmID]
  
  ## (a2) Not duplicated team names
  tmID1B <- tmID[!tmID %in% spboTeamID]
  tmp.spbo1B <- spboTeamID[!spboTeamID %in% tmID]
  
  ## (b1) Partial match team names, only capital letter difference
  tmID2A <- tmID1B[tolower(tmID1B) %in% tolower(tmp.spbo1B)]
  tmp.spbo2A <- tmp.spbo1B[tolower(tmp.spbo1B) %in% tolower(tmID1B)]
  
  ## (b2) Partial match team names, not only capital letter difference
  tmID2B <- tmID1B[!tmID1B %in% tmID2A]
  tmp.spbo2B <- tmp.spbo1B[!tmp.spbo1B %in% tmp.spbo2A]
  
  ## 2) Apply regular expression to match the most approximate matching team names
  ##
  ## We need to seperate the B team or U21 etc since it will be the most approximate team names.
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
  ## separate the second team or U18/U19 team etc.
  ##
  ## (a1) Separate second team, reserved team or U18/U21 etc team names
  ext <- c(' 2',' 3',' 4',' 5',' 6',' 7',' II',' III',' IV',' V',' VI',' VII',' B',' C',' D',paste0(' U',seq(15,23)),' RES','REV','RESERVED')
  tmID3A <- tmID2B[(tolower(substr(tmID2B,nchar(tmID2B)-7,nchar(tmID2B))) %in% tolower(ext))|
                   (tolower(substr(tmID2B,nchar(tmID2B)-3,nchar(tmID2B))) %in% tolower(ext))|
                   (tolower(substr(tmID2B,nchar(tmID2B)-2,nchar(tmID2B))) %in% tolower(ext))|
                   (tolower(substr(tmID2B,nchar(tmID2B)-1,nchar(tmID2B))) %in% tolower(ext))]
  
  tmp.spbo3A <- tmp.spbo2B[(tolower(substr(tmp.spbo2B,nchar(tmp.spbo2B)-7,nchar(tmp.spbo2B))) %in% tolower(ext))|
                           (tolower(substr(tmp.spbo2B,nchar(tmp.spbo2B)-3,nchar(tmp.spbo2B))) %in% tolower(ext))|
                           (tolower(substr(tmp.spbo2B,nchar(tmp.spbo2B)-2,nchar(tmp.spbo2B))) %in% tolower(ext))|
                           (tolower(substr(tmp.spbo2B,nchar(tmp.spbo2B)-1,nchar(tmp.spbo2B))) %in% tolower(ext))]
  
  ## (a2) Not second team, reserved team or U18/U21 etc team names
  tmID3B <- tmID2B[!tmID2B %in% tmID3A]
  tmp.spbo3B <- tmp.spbo2B[!tmp.spbo2B %in% tmp.spbo3A]
  
  df1 <- data.frame(teamID1=c(tmID1,tmID2A,tmID2B),spboTeamID=c(tmp.spbo1,rep(NA,length(tmID2))),
                   Match=c(rep('Duplicated',length(tmID1)),rep('Partial',length(tmID2))))
  
  ## 2) Apply regular expression to match the most approximate matching team names
  fildata1 <- mbase$datasets %>% mutate(.,Date=as.Date(DateUK),Home=as.character(Home),Away=as.character(Away)) %>% 
              filter(.,((Home %in% tmID2)&(!Home %in% mbase$others)), ((Away %in% tmID2)&(!Away %in% mbase$others))) %>% 
              select(.,Date,Home,Away) %>% arrange(.,Date)
  #'@ fildata2 <- unique(melt(data.frame(fildata1), id.vars=c('Date'))[c('Date','value')])
  fildata2 <- fildata1 %>% gather(Group, Team, Home:Away) %>% select(Date,Team) %>% .[!duplicated(.),] %>% spread(Team,Team) %>% tbl_df 
  rownames(fildata2) <- fildata2$Date; fildata2 <- fildata2 %>% select(-Date)
  
  fildataA <- spboData %>% mutate(.,Date=as.Date(DateUK),Home=as.character(Home),Away=as.character(Away)) %>% 
              filter(.,((Home %in% tmp.spbo)&(!Home %in% mbase$others)), ((Away %in% tmp.spbo)&(!Away %in% mbase$others)),
              Date %in% fildata1$Date) %>% select(.,Date,Home,Away) %>% arrange(.,Date)
  #'@ fildataB <- unique(melt(data.frame(fildataA), id.vars=c('Date'))[c('Date','value')])
  fildataB <- fildataA %>% gather(Group, Team, Home:Away) %>% select(Date,Team) %>% .[!duplicated(.),] %>% spread(Team,Team) %>% tbl_df
  rownames(fildataB) <- fildataB$Date; fildataB <- fildataB %>% select(-Date)
  
  
  
  ## List the number of duplicated/matching and partial/approximated matching team names
  tbl_df(data.frame(teamID=as.numeric(sapply(list(tmID,tmID1A,tmID1B,tmID2A,tmID2B,tmID3A,tmID3B),length)),
         spboTeamID=as.numeric(sapply(list(spboTeamID,tmp.spbo1A,tmp.spbo1B,tmp.spbo2A,tmp.spbo2B,tmp.spbo3A,tmp.spbo3B),length))))
  
  ## ---------------------------------------------------------------------------------
  ## http://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets
  ## http://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/
  
  mutate(fildata2, fildataB)
  #'@ fildata2 %>% spread(Team,Team)
  #'@ anti_join(fildata2,fildataB)
  llply(seq(nrow(fildata2)), function(i) llply(seq(ncol(fildata2)), function(j){
    grep(fildata2[i,j],fildataB[i,])}),.parallel=TRUE)
  ## http://stackoverflow.com/questions/24561936/grep-to-search-column-names-of-a-dataframe
  grep(fildata2, fildataB, value=TRUE)
  
  ## 2) Apply regular expression to match the most approximate matching team names
  scd <- rep(list(c('II','III','IV','V','VI','VII','B','C','D',paste0('U',seq(15,23)))),length(tmID2))
  y <- y2 <- list()
  tm2 <- as.vector(as.character(llply(seq(scd), function(i){
    unique(unlist(llply(as.list(seq(scd[[i]])), function(j){
      y[i] <- tmp.spbo[grep(tmID2[i], tmp.spbo, ignore.case=TRUE)]
      y2[i] <- y[i][!gsub(paste(tmID2[i],''),'\\1',y[i]) %in% scd[[i]][j]]
      unique(unlist(y2[i][!str_detect(y2[i],scd[[i]][j])]))
    },.parallel=TRUE)))
  },.parallel=TRUE))); rm(y, y2, scd)
  
  scd <- c('II','III','IV','V','VI','VII','B','C','D',paste0('U',seq(15,23)))
  y <- y2 <- list()
  tm2 <- llply(as.list(seq(tmID2)), function(i){
           y[i] <- tmp.spbo[grep(tmID2[[i]], tmp.spbo, ignore.case=TRUE)]
           y2[i] <- y[i][!gsub(paste(tmID2[[i]],''),'\\1',y[i]) %in% scd]
           unique(unlist(y2[i][!str_detect(y2[i],scd)]))
       },.parallel=TRUE)
  
  
  ## 3) Apply stringdist() to match the most approximate matching team names
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
























