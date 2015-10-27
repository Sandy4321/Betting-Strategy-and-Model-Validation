scrapSPBO <- function(lnk=lnk, dateID=dateID, path=path, parallel=TRUE){
  ## Setting to omitt all warnings since there are alot of data validation for manipulation
  options(warn=-1)
  
  lnk <- lnk; dateID <- dateID; path <- path; parallel <- parallel
  if(length(lnk)!=length(dateID)){ stop('The length of url links and date id not tally !!!') }
  n <- seq(1:length(lnk))
  
  ## Web scrapping on leagues and teams
  ## Loading the packages
  if(!suppressWarnings(require('BBmisc',quietly=TRUE))){
    suppressWarnings(install.packages('BBmisc'))
    suppressAll(require('BBmisc',quietly=TRUE))}
  
  ## Might load RSeleniumUtilities package if needed
  ##  https://github.com/greenore/RSeleniumUtilities
  suppressPackageStartupMessages(library('BBmisc'))
  pkgs <- c('reshape','plyr','dplyr','rJava','RSelenium','rvest','XML','RCurl','stringr','stringi','doParallel','grDevices')
  suppressPackageStartupMessages(lib(pkgs)); rm(pkgs)
  
  ## Set parallel computing
  #'@ cl <- makePSOCKcluster(3)
  registerDoParallel(cores=3)
  ## http://www.inside-r.org/r-doc/parallel/detectCores
  #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  
  ## http://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  #'@ registerDoSEQ()
  #'@ unregister <- function() {
  #'@   env <- foreach:::.foreachGlobals
  #'@   rm(list=ls(name=env), pos=env)}
  
  ## Load arrDataElem function
  source(paste0(getwd(),'/function/arrDataElem.R'))
  
  vbase <- llply(n,function(i){
    dataElem <- html_session(lnk[i]) %>% html_nodes('script') %>% .[[1]] %>% html_text %>% str_split(',') %>%
      .[[1]] %>% str_extract_all(.,'[#0-9a-zA-Z].*') %>% .[sapply(.,length)==1] %>%
      .[!str_detect(.,'[\u4e00-\u9fa5]')] %>% unlist %>% gsub('(var bf=\")|(0!)','',.) %>% .[!str_detect(.,'=')]
    ## we can apply regular expression on color codes as well if any, but keep it easier for further data filtering if needed
    #' rgb(tab, maxColorValue=256)
    
    ## ------------------------------------------------------------------------------------------------------------------
    ## we can adjust how many columns we needed, but the ncol of scrapped data (postponed matches) difference.
    ##   Therefore here we use 15 and split the suspended matches into 2nd list, 3rd postponed, 4th other sports 
    #'@ n.data <- unlist(sapply(1:length(dataElem), function(i) i[length(dataElem)%%i==0])) # series of ncol divided by length of default dataElem
    
    if(length(dataElem)>0){
      df1 <- arrDataElem(dataElem=dataElem,dateID=dateID,i=i,n.col=15)
    }else{
      df1 <- list(data=na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
             'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2'))))),n=0)}
    
    ## Suspended soccer matches has 13,14,15 columns
    if(df1$n<(length(dataElem)-1)){
      df1.sps <- llply(13:15,function(x) arrDataElem(dataElem[(df1$n+1):length(dataElem)],dateID=dateID,i=i,n.col=x), .parallel=parallel)
      df1.sps <- list(data=Reduce(function(x,y) {merge(x,y,all=TRUE)}, llply(df1.sps,function(x) x[[1]])),n=sum(sapply(df1.sps,function(x) x[[2]])))
    }else{
      df1.sps <- list(data=na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
                 'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2'))))),n=0)}
    
    ## Postponed soccer matches has 11 columns & Date==0
    if(sum(df1$n,df1.sps$n)<(length(dataElem)-1)){
      df1.pst <- arrDataElem(dataElem=dataElem[(sum(df1$n,df1.sps$n)+1):length(dataElem)],dateID=dateID,i=i,n.col=11)
    }else{
      df1.pst <- list(data=na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
                 'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2'))))),n=0)}
    
    ## ------------------------------------------------------------------------------------------------------------------
    ## Completed other sports matches has 15 columns
    if(sum(df1$n,df1.sps$n,df1.pst$n)<(length(dataElem)-1)){
      df2 <- arrDataElem(dataElem=dataElem[(sum(df1$n,df1.sps$n,df1.pst$n)+1):length(dataElem)],dateID=dateID,i=i,n.col=15)
    }else{
      df2 <- list(data=na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
             'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2'))))),n=0)}
    
    ## Suspended other sports matches has 13,14,15 columns
    if(sum(df1$n,df1.sps$n,df1.pst$n,df2$n)<(length(dataElem)-1)){
      df2.sps <- llply(13:15,function(x) arrDataElem(dataElem[(sum(df1$n,df1.sps$n,df1.pst$n,df2$n)+1):length(dataElem)],
                                                 dateID=dateID,i=i,n.col=x), .parallel=parallel)
      df2.sps <- list(data=Reduce(function(x,y) {merge(x,y,all=TRUE)}, llply(df2.sps,function(x) x[[1]])),n=sum(sapply(df2.sps,function(x) x[[2]])))
    }else{
      df2.sps <- list(data=na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
                 'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2'))))),n=0)}
    
    ## Postponed other sports matches has 11 columns & Date==0
    if(sum(df1$n,df1.sps$n,df1.pst$n,df2$n,df2.sps$n)<(length(dataElem)-1)){
      df2.pst <- arrDataElem(dataElem=dataElem[(sum(df1$n,df1.sps$n,df1.pst$n,df2$n,df2.sps$n)+1):length(dataElem)],dateID=dateID,i=i,n.col=11)
    }else{
      df2.pst <- list(data=na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
                 'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2'))))),n=0)}
    
    ## ------------------------------------------------------------------------------------------------------------------
    ## Handle a list of soccer and other sports matches
    if(sum(df1$n, df1.sps$n, df1.pst$n)>0){
      socData <- data.frame(Sports='soccer',Reduce(function(x,y) {merge(x,y,all=TRUE)}, llply(list(df1,df1.sps,df1.pst),function(x) x[[1]])))
    }else{
      socData <- na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
                 'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')))))}
    if(sum(df2$n, df2.sps$n, df2.pst$n)>0){
      othData <- data.frame(Sports='others',Reduce(function(x,y) {merge(x,y,all=TRUE)}, llply(list(df2,df2.sps,df2.pst),function(x) x[[1]])))
    }else{
      othData <- na.omit(data.frame(matrix(NA,ncol=16,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date','Finished','Home',
                 'Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')))))}
    dfm <- merge_all(list(socData,othData)); rm(socData, othData)
    dfm <- dfm[!duplicated(dfm),]
    
    ## Save livescores data into folder
    dir.create(file.path(paste0(getwd(),'/datasets/',path)))
    
    ## Introduce to ff and ffbase for large datasets
    ## http://www.r-bloggers.com/if-you-are-into-large-data-and-work-a-lot-with-package-ff/
    ## In order to easier manipulate (read/write) the data, here I just keep the csv file seperately by dateID.
    write.csv(dfm, file=paste0(getwd(),'/datasets/',path,'/',dateID[i],'.csv'))
    cat(i,paste0(' Wrote ',dateID[i],'.csv'),'\n')
    return(dfm)})
  #' @teamID <- sort(factor(unique(c(unique(as.character(vbase$Home)),unique(as.character(vbase$Away))))))
  #' @stopCluster(cl)
    
  #' @return(list(lnk=lnk, dateID=dateID, teamID=teamID, mbase=vbase))
}


