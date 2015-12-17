scrapSPBO <- function(lnk=lnk, dateID=dateID, path=path){
  ## Setting to omitt all warnings since there are alot of data validation for manipulation
  options(warn=-1)

  if(length(lnk)!=length(dateID)){ stop('The length of url links and date id not tally !!!') }
  i = length(lnk)

  ## Web scrapping on leagues and teams
  ## Loading the packages
  if(!suppressWarnings(require('BBmisc',quietly=TRUE))){
    suppressWarnings(install.packages('BBmisc'))
    suppressAll(require('BBmisc',quietly=TRUE))}

  ## Might load RSeleniumUtilities package if needed
  ##  https://github.com/greenore/RSeleniumUtilities
  suppressPackageStartupMessages(library('BBmisc'))
  pkgs <- c('plyr','dplyr','rJava','RSelenium','rvest','XML','RCurl','stringr','stringi','doParallel','grDevices','doParallel')
  suppressPackageStartupMessages(lib(pkgs))

  ## Set parallel computing
  registerDoParallel(cores = 3)
  ## http://www.inside-r.org/r-doc/parallel/detectCores
  #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))

  ## http://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster
  #'@ registerDoSEQ()
  #'@ unregister <- function() {
  #'@   env <- foreach:::.foreachGlobals
  #'@   rm(list=ls(name=env), pos=env)}
  
  #'@ df1 %>% mutate_each(funs(as.character))
  vbase <- llply(i,function(i){
    dataElem <- html_session(lnk[i]) %>% html_nodes('script') %>% .[[1]] %>% html_text %>% str_split(',') %>%
              .[[1]] %>% str_extract_all(.,'[#0-9a-zA-Z].*') %>% .[sapply(.,length)==1] %>%
              .[!str_detect(.,'[\u4e00-\u9fa5]')] %>% unlist %>% gsub('(var bf=\")|(0!)','',.) %>% .[!str_detect(.,'=')]
    ## we can apply regular expression on color codes as well if any, but keep it easier for further data filtering if needed
    #'@ rgb(tab, maxColorValue=256)

    ## we can adjust how many columns we needed, but the ncol of scrapped data (postponed matches) difference.
    ##   Therefore here we use 15 and split the postponed matches into 2nd list, Other sports to 3rd list
    #'@ n.data <- unlist(sapply(1:length(dataElem), function(i) i[length(dataElem)%%i==0]))
    if(length(dataElem)>0){
      df1 <- dataElem %>% matrix(.,ncol=15,byrow=TRUE,dimnames=list(NULL,c('matchID','LeagueColor','League','Date',
             'Finished','H.Card','Home','FTHG','FTAG','Away','A.Card','HTGoal','HT.matchID','HT.graph1','HT.graph2'))) %>%
             data.frame %>% subset(.,nchar(as.numeric(as.character(.$matchID)))==10)

      if(nrow(df1)>0){
        ## change the class type to filter out the non-completed or other sports matches
        df1$matchID <- as.numeric(as.character(df1$matchID))
        df1$LeagueColor <- factor(as.character(df1$LeagueColor))
        df1$League <- factor(as.character(df1$League))
        df1$Date <- strptime(paste0(substr(dateID[i],1,4),'/',as.character(df1$Date)), format='%Y/%m/%d %H:%M', tz='HKT')
        df1$Finished <- as.numeric(as.character(df1$Finished))
        df1$H.Card <- as.numeric(as.character(df1$H.Card))
        df1$Home <- factor(as.character(df1$Home))
        df1$FTHG <- as.numeric(as.character(df1$FTHG))
        df1$FTAG <- as.numeric(as.character(df1$FTAG))
        df1$Away <- factor(as.character(df1$Away))
        df1$A.Card <- factor(as.character(df1$A.Card))
        mHTGoal <- data.frame(str_split_fixed(as.character(df1$HTGoal),'-',2))
        names(mHTGoal) <- c('HTHG','HTAG')
        df1 <- data.frame(df1[c('matchID','LeagueColor','League','Date','Finished','Home','Away','FTHG','FTAG')],mHTGoal,
               df1[c('H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')])
        df1$HTHG <- as.numeric(as.character(df1$HTHG))
        df1$HTAG <- as.numeric(as.character(df1$HTAG))
        df1$HT.matchID <- as.numeric(as.character(df1$HT.matchID))
        df1$HT.graph1 <- as.numeric(as.character(df1$HT.graph1))
        df1$HT.graph2 <- as.numeric(as.character(df1$HT.graph2))
        rm(mHTGoal)
        df1 <- na.omit(df1)
      }else{
        df1 <- data.frame()
      }
    }else{
      df1 <- data.frame()}
    
    ## Length of completed soccer matches element (before str_split on HTGoals)
    n.df1 <- length(unlist(sapply(df1[!names(df1)%in%'HTAG'], as.character)))
    n.df1 == prod(dim(df1[!names(df1)%in%'HTAG']))
    
    ## Postponed soccer matches has 11 columns
    if(n.df1<length(dataElem)){
      df2 <- dataElem[(n.df1+1):length(dataElem)] %>% matrix(.,ncol=11,byrow=TRUE,dimnames=list(NULL,
             c('matchID','LeagueColor','League','Date','Finished','Home','Away','HTGoal','HT.matchID','HT.graph1',
             'HT.graph2'))) %>% data.frame %>% subset(.,nchar(as.numeric(as.character(.$matchID)))==10)
    
      if(nrow(df2)>0){
        ## change the class type to filter out the non-completed or other sports matches
        df2$matchID <- as.numeric(as.character(df2$matchID))
        df2$LeagueColor <- factor(as.character(df2$LeagueColor))
        df2$League <- factor(as.character(df2$League))
        df2$Date <- strptime(paste0(substr(dateID[i],1,4),'/',as.character(df2$Date)), format='%Y/%m/%d %H:%M', tz='HKT')
        df2$Finished <- as.numeric(as.character(df2$Finished))
        df2$Home <- factor(as.character(df2$Home))
        df2$Away <- factor(as.character(df2$Away))
        df2$HTGoal <- as.numeric(as.character(df2$HTGoal))
        df2$HT.matchID <- as.numeric(as.character(df2$HT.matchID))
        df2$HT.graph1 <- as.numeric(as.character(df2$HT.graph1))
        df2$HT.graph2 <- as.numeric(as.character(df2$HT.graph2))
        df2 <- na.omit(df2)
      }else{
        df2 <- data.frame()
      }
    }else{
      df2 <- data.frame()}
    
    ## Length of completed soccer matches element (before str_split on HTGoals)
    n.df2 <- length(unlist(sapply(df2, as.character)))
    n.df2 == prod(dim(df2))

    ## Completed other sports matches has 15 columns
    if(sum(n.df1,n.df2)<length(dataElem)){
      df3 <- dataElem[(n.df1+n.df2+1):length(dataElem)] %>% matrix(.,ncol=15,byrow=TRUE, dimnames=list(NULL,
             c('matchID','LeagueColor','League','Date','Finished','H.Card','Home','FTHG','FTAG','Away','A.Card',
             'HTGoal','HT.matchID','HT.graph1','HT.graph2'))) %>% data.frame %>% subset(.,nchar(as.numeric(
             as.character(.$matchID)))==10)
    
      if(nrow(df3)>0){
        ## change the class type to get only other sports completed matches
        df3$matchID <- as.numeric(as.character(df3$matchID))
        df3$LeagueColor <- factor(as.character(df3$LeagueColor))
        df3$League <- factor(as.character(df3$League))
        df3$Date <- strptime(paste0(substr(dateID[i],1,4),'/',as.character(df3$Date)), format='%Y/%m/%d %H:%M', tz='HKT')
        df3$Finished <- as.numeric(as.character(df3$Finished))
        df3$H.Card <- as.numeric(as.character(df3$H.Card))
        df3$Home <- factor(as.character(df3$Home))
        df3$FTHG <- as.numeric(as.character(df3$FTHG))
        df3$FTAG <- as.numeric(as.character(df3$FTAG))
        df3$Away <- factor(as.character(df3$Away))
        df3$A.Card <- factor(as.character(df3$A.Card))
        mHTGoal <- data.frame(str_split_fixed(as.character(df3$HTGoal),'-',2))
        names(mHTGoal) <- c('HTHG','HTAG')
        df3 <- data.frame(df3[c('matchID','LeagueColor','League','Date','Finished','Home','Away','FTHG','FTAG')],mHTGoal,
             df3[c('H.Card','A.Card','HT.matchID','HT.graph1','HT.graph2')])
       df3$HTHG <- as.numeric(as.character(df3$HTHG))
        df3$HTAG <- as.numeric(as.character(df3$HTAG))
        df3$HT.matchID <- as.numeric(as.character(df3$HT.matchID))
        df3$HT.graph1 <- as.numeric(as.character(df3$HT.graph1))
        df3$HT.graph2 <- as.numeric(as.character(df3$HT.graph2))
        rm(mHTGoal)
        df3 <- na.omit(df3)
      }else{
        df3 <- data.frame()
      }
    }else{
      df3 <- data.frame()}
    
    ## Length of completed other sports matches element (before str_split on HTGoals)
    n.df3 <- length(unlist(sapply(df3[!names(df3)%in%'HTAG'], as.character)))
    n.df3 == prod(dim(df3[!names(df3)%in%'HTAG']))
    
    ## Postponed other sports matches has 11 columns
    if(sum(n.df1,n.df2,n.df3)<length(dataElem)){
      df4 <- dataElem[(n.df1+n.df2+n.df3+1):length(dataElem)] %>% matrix(.,ncol=11,byrow=TRUE,dimnames=list(NULL,
             c('matchID','LeagueColor','League','Date','Finished','Home','Away','HTGoal','HT.matchID','HT.graph1',
             'HT.graph2'))) %>% data.frame %>% subset(.,nchar(as.numeric(as.character(.$matchID)))==10)
    
      if(nrow(df3)>0){
        ## change the class type to filter out the non-completed or other sports matches
        df4$matchID <- as.numeric(as.character(df4$matchID))
        df4$LeagueColor <- factor(as.character(df4$LeagueColor))
        df4$League <- factor(as.character(df4$League))
        df4$Date <- strptime(paste0(substr(dateID[i],1,4),'/',as.character(df4$Date)), format='%Y/%m/%d %H:%M', tz='HKT')
        df4$Finished <- as.numeric(as.character(df4$Finished))
        df4$Home <- factor(as.character(df4$Home))
        df4$Away <- factor(as.character(df4$Away))
        df4$HTGoal <- as.numeric(as.character(df4$HTGoal))
        df4$HT.matchID <- as.numeric(as.character(df4$HT.matchID))
        df4$HT.graph1 <- as.numeric(as.character(df4$HT.graph1))
        df4$HT.graph2 <- as.numeric(as.character(df4$HT.graph2))
        df4 <- na.omit(df4)
      }else{
        df4 <- data.frame()
      }
    }else{
      df4 <- data.frame()}
    
    ## Length of completed soccer matches element (before str_split on HTGoals)
    n.df4 <- length(unlist(sapply(df4, as.character)))
    n.df4 == prod(dim(df4))
    
    ## Handle a list of soccer and other sports matches
    if(nrow(df1)>0){
      df1 <- data.frame(Sports='Soccer',df1)
    }else{ df1 <- data.frame()}
    if(nrow(df2)>0){
      df2 <- data.frame(Sports='Soccer',df2)
    }else{ df2 <- data.frame()}
    if(nrow(df3)>0){
      df3 <- data.frame(Sports='Others',df3)
    }else{ df3 <- data.frame()}
    if(nrow(df4)>0){
      df4 <- data.frame(Sports='Others',df4)
    }else{ df4 <- data.frame()}
    
    ## Postponed matches
    df.post <- rbind(df2,df4)
    if(nrow(df.post)>0){
      df.post <- data.frame(df.post[c('Sports','matchID','LeagueColor','League','Date','Finished','Home','Away')],
                 FTHG=NA,FTAG=NA,HTHG=NA,HTAG=NA,H.Card=NA,A.Card=NA,df.post[c('HT.matchID','HT.graph1','HT.graph2')])
    }else{ df.post <- data.frame()}
    
    ## dfm <- rbind_all(list(df1,df3,df.post)) #Error: POSIXlt not supported
    dfm <- Reduce(function(x,y) {merge(x,y,all=TRUE)}, list(df1,df3,df.post))
    if(nrow(dfm)>0){
      dfm <- dfm[order(dfm$Date, decreasing=FALSE) & order(dfm$Finished, decreasing=FALSE),]; row.names(dfm) <- NULL
      dfm$Finished <- ifelse(dfm$Finished==4,'End',ifelse(dfm$Finished==0,'Postponed',NA))
    }else{ dfm <- data.frame()}
    rm(dataElem,df,df1,df2,df3,df4)
    
    ## Save livescores data into folder
    dir.create(file.path(paste0(getwd(),'/datasets/',path)))
    
    ## Introduce to ff and ffbase for large datasets
    ## http://www.r-bloggers.com/if-you-are-into-large-data-and-work-a-lot-with-package-ff/
    ## In order to easier manipulate (read/write) the data, here I just keep the csv file seperately by dateID.
    write.csv(dfm, file=paste0(getwd(),'/datasets/',path,'/',dateID,'.csv'))
    return(dfm)})
  teamID <- system.time(sort(factor(unique(c(unique(as.character(vbase[[1]]$Home)),unique(as.character(vbase[[1]]$Away)))))))
  stopCluster()

  return(list(lnk=lnk, dateID=dateID, teamID=teamID, mbase=vbase))}

  
  
#############################################################
## Scrape the leagues and also overrounds which provides by a sportsbookmaker named Firm B.
##   Besides, need to scrap the final-scores / half-time scores / result of soccer matches
teamID <- sort(unique(c(as.character(mbase$Home), as.character(mbase$Away))))
dateID <- sort(unique(mbase$Date)); spboDate <- gsub('-','',dateID)
#' @lnk1 <- paste0('http://data2.7m.cn/result_data/default_en.shtml?date=',dateID)
lnk2 <- paste0('http://www8.spbo.com/history.plex?day=',spboDate,'&l=en')
## http://www8.spbo.com/history.plex?l=en&day=20110107
#' @lnk3 <- 'http://data.nowgoal.com/history/handicap.htm' #this website content includes the odds price

  
## web scrapping
## https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-headless.html
## https://github.com/eugeneware/phantomjs-bin
## https://www.npmjs.com/package/phantomjs #Error Listing and solutions
pJS <- phantom(paste0(getwd(),'/phantomjs'))
#' @pJS <- phantom()
#' @Sys.sleep(5) # give the binary a moment
webDr <- remoteDriver(browserName = paste0(getwd(),'/phantomjs'))
webDr$open(silent=TRUE)
webDr$navigate(lnk3) ## for loop

## ========================================================================
## rvest html_form, need to learn about checkbox, button, captcha etc.
## XML and Web Technologies for Data Sciences with R --- by Duncan Temple Lang
##    chapter 9 - Scraping Data from HTML Forms
##    library(RHTMLForms) # while no update
##    used to read getForm() and postForm, GET and POST but dont capture how does it works
#' @sess <- html_session(lnk)
#' @f0 <- sess %>% html_form
#' @f1 <- set_values(f0[[2]], 'matchdate'=dateID[1], 'companyid1'=list(c(3,8,4,12,1,23,24,17,31,14,35,22)))
#' @s <- submit_form(sess, f1)
#' @
#' @f1 <- set_values(f0[[1]],'button'=FALSE,'button'=TRUE,'button'=TRUE)
#' @s <- submit_form(sess, f1)

## ========================================================================
## library(RSelenium) # using selenium and phantomjs on winx86_64 since Linux doesnt work
scrapOdds <- function(lnk=lnk, dateID=dateID){
  ## lnk3 <- 'http://data.nowgoal.com/history/handicap.htm' #this website content includes the odds price
  llply(seq(1,length(dateID)), function(i){
    webDr$findElement(using='xpath', value='//*[@id="matchdate"]')$sendKeysToElement(list(dateID[i],'\uE007'))
    webDr$findElement(using='xpath', value='//*[@id="frmSearch"]/input[3]')$clickElement()
    webDr$findElement(using='xpath', value='//*[@id="main"]/ul/li[2]/a')$clickElement()
    ##
    #' @webDr$findElement(using='xpath', value='//*[@id="button1"]')$clickElement()
    #' @webDr$findElement(using='xpath', value='//*[@id="button0"]')$clickElement()
    web <- webDr$getPageSource()[[1]]
    #' @tab <- html_session(web) %>% html_nodes('a') ##doesn't work

    ## sapply(strsplit("John Davis", " "), "]", 2)
    tab <- suppressWarnings(rbind_all(readHTMLTable(htmlParse(web), header=TRUE)))
    tabb <- tab
    dateMatch <- ldply(rep(str_split(tab$League[1], ' \\('),nrow(tab)-2), .parallel=TRUE)
    dateMatch$V1 <- as.Date(strptime(dateMatch$V1,format='%m/%d/%Y',tz='GMT'))
    dateMatch$V2 <- gsub(')','',dateMatch$V2)
    tabb <- tabb[3:nrow(tabb),]; tab <- tab[3:nrow(tab),]
    tabb$League <- sapply(str_extract_all(as.character(tabb$League), '[A-Za-z]{1,} [A-Za-z]{1,}'),function(x) x)
    tabb$Time <- sapply(str_extract_all(as.character(tab$League),'[0-9]{1,}:[0-9]{1,}'),function(x) x)
    teamMatch <- ldply(strsplit(as.character(tabb$Team), '   '), .parallel=TRUE)
    teamMatch$V1 <- str_replace_all(as.character(teamMatch$V1),'[[:space:]]${1,}','')
    scMatch <- ldply(str_split(as.character(tabb$FT),'-'), .parallel=TRUE)
    scMatch$V2.1 <- ifelse(nchar(scMatch$V2)==2,substr(scMatch$V2,1,1),
                    ifelse(nchar(scMatch$V2)==3&nchar(scMatch$V1)==1,substr(scMatch$V2,1,1),
                    ifelse(nchar(scMatch$V2)==3&nchar(scMatch$V1)==2,substr(scMatch$V2,1,2),
                    ifelse(nchar(scMatch$V2)==4&nchar(scMatch$V1)==2,substr(scMatch$V2,1,2)))))
    scMatch$V2.2 <- ifelse(nchar(scMatch$V2)==2,substr(scMatch$V2,2,2),
                    ifelse(nchar(scMatch$V2)==3&nchar(scMatch$V3)==1,substr(scMatch$V2,3,3),
                    ifelse(nchar(scMatch$V2)==3&nchar(scMatch$V3)==2,substr(scMatch$V2,2,3),
                    ifelse(nchar(scMatch$V2)==4&nchar(scMatch$V3)==2,substr(scMatch$V2,3,4)))))
    scMatch <- scMatch[c(1,4,5,3)]
    dMatch <- data.frame(dateMatch,tabb[c('Time','League')],teamMatch,scMatch)
    names(dMatch) <- c('Date','Day','Time','League','Team1_Fav','Team2_Und','FT1','FT2','HT1','HT2')
    oddsMatch <- llply(tab[4:ncol(tab)], function(x) {y=as.character(x);
                 df=data.frame(X1=substr(y,5,nchar(y)-4),Odds1=as.numeric(substr(y,1,4)),
                 Odds2=as.numeric(substr(y,nchar(y)-3,nchar(y))));
                 df1=data.frame(str_split_fixed(df$X1,'/',2));
                 df1$X1=as.numeric(gsub(' ','',df1$X1));df1$X2=as.numeric(gsub(' ','',df1$X2));
                 df1$X2=ifelse(is.numeric(df1$X1)&is.na(df1$X2),df1$X1,df1$X2);
                 data.frame(dMatch,HCap=ifelse(df1$X2+df1$X1==0,0,-(df1$X2+df1$X1)/2),df[-1])}, .parallel=TRUE)
    rm(tab,tabb,dateMatch,scMatch)
    oddsMatch})}
stopCluster()





oddsMatch <- llply(tab[4:ncol(tab)], function(x) str_split(x,'(/)| '))
oddsMatch <- llply(tab[4:ncol(tab)], function(x) str_extract_all(x,'([0-9]{1,}.[0-9]{1,})|([0-9]{1,}/[0-9]{1,})'))


## =============================================================

