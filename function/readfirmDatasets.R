readfirmDatasets <- function(years=years, parallel=FALSE){
  library('BBmisc')
  pkgs <- c('plyr','dplyr','stringr','lubridate')
  suppressAll(lib(pkgs)); rm(pkgs)
  
  if(parallel==TRUE){
    ## Preparing the parallel cluster using the cores
    ## Set parallel computing
    suppressPackageStartupMessages(require('doParallel',quietly=TRUE))
    #'@ cl <- makePSOCKcluster(3)
    doParallel::registerDoParallel(cores = 3)
    #' @BiocParallel::register(MulticoreParam(workers=2))
    ## http://www.inside-r.org/r-doc/parallel/detectCores
    #'@ doParallel::registerDoParallel(makeCluster(detectCores(logical=TRUE)))
  }
  
  if(is.numeric(years)){
    years <- as.list(years)
  }else if(is.list(years)){
    if(is.numeric(unlist(as.list(years)))){
      years <- years
    }else{
      stop('Please insert a list or a vector of years in numeric format!')
    }
  }else{
    stop('Please insert a list or a vector of years in numeric format!')
  }
  ## Read the datasets
  ## Refer to **Testing efficiency of coding.Rmd** at chunk `get-data-summary-table-2.1`
  dfm <- rbind_all(llply(years, function(x) data.frame(Sess=x,read.csv(paste0(getwd(),'/datasets/',x,'.csv'),header=TRUE,sep=',')),.parallel=parallel))
  
  ## Data processing and clearing
  matchID <- str_extract_all(as.character(dfm$Match),'[[:alnum:]\\(.\\) ]{1,}[[:alnum:].: ]{1,}')
  
  ## Checking if the strings length match
  #' @laply(matchID,length)
  #' @which(laply(matchID,length)!=4)
  #' @matchID[laply(matchID,length)!=4]
  
  matchID <- tbl_df(ldply(matchID,.parallel=TRUE))
  matchID$V1 <- tbl_df(ldply(str_split(matchID$V1,' vs '),.parallel=TRUE))
  matchID$V2 <- str_replace_all(matchID$V2,'\\(','')
  matchID <- data.frame(matchID$V1,matchID[-1])
  matchID[str_detect(matchID$V2,'\\('),]$V2 <- paste0(matchID[str_detect(matchID$V2,'\\('),]$V2,')')
  
  ## Omit the (Corners), (1st Half Corners) and (1st Half)
  others <- sort(unique(c(matchID[str_detect(matchID$V1,'(1st Half)|(Corners)|(1st Half Corners)'),]$V1,
                          matchID[str_detect(matchID$V2,'(1st Half)|(Corners)|(1st Half Corners)'),]$V2)))
  corners <- sort(unique(c(as.character(matchID[str_detect(matchID$V1,'(Corners)'),]$V1),
                           as.character(matchID[str_detect(matchID$V2,'(Corners)'),]$V2))))
  
  #'@ options(dplyr.print_max = 1e9)
  dfm$Stake <- as.numeric(gsub('[^0-9]','',dfm$Stake))
  #'@ InPlay <- str_extract_all(as.character(dfm$In.R.),'[^\\?(]{1,}[0-9a-zA-Z]{1,}')
  InPlay <- str_extract_all(as.character(dfm$In.R.),'[^\\?]{1,}[0-9a-zA-Z]{1,}')
  mx <- max(laply(InPlay, length))
  InPlay <- ldply(InPlay, function(x) rep(x,mx)[1:mx])
  InPlay$V2 <- gsub('[^0-9a-zA-Z]','',InPlay$V2)
  dfm$P...L <- as.numeric(gsub('[^0-9]','',dfm$P...L))
  dfm$Match <- NULL
  dfm <- data.frame(cbind(dfm[c(2:1)],matchID,dfm[3:6],InPlay,dfm[8:ncol(dfm)]))
  names(dfm) <- c('No','Sess','Home','Away','Day','Date','Time','Selection','HCap','EUPrice','Stakes','CurScore','Mins','Result','PL','Rebates')
  dfm$Home <- factor(dfm$Home); dfm$Away <- factor(dfm$Away); dfm$Day <- factor(dfm$Day)
  ## tbl_df() doesn't support POSIXct format
  ## https://github.com/hadley/dplyr/issues/1382
  dfm$DateUK <- strptime(paste(as.character(dfm$Date),as.character(dfm$Time)),format='%d %b %Y %H:%M',tz='Europe/London') #Convert to POSIXct format
  dfm$DateUK <- format(as.POSIXct(dfm$DateUK,tz='Europe/London'), tz='Europe/London',usetz=TRUE) #Convert to character format
  dfm$Date <- as.Date(strptime(dfm$Date,format='%d %b %Y',tz='GMT'))
  dfm$Time <- factor(dfm$Time)
  #'@ levels(as.Date(dfm$Date)==as.character(dfm$Date)) #Checking if as.Date(x) same with GMT time zone
  dfm$Selection <- factor(dfm$Selection)
  dfm$CurScore <- factor(dfm$CurScore)
  dfm$Mins <- factor(dfm$Mins)
  dfm$PL <- ifelse(dfm$Result=='Loss', -dfm$PL, dfm$PL)
  dfm$Return <- dfm$Stakes+dfm$PL
  dfm$HKPrice <- dfm$EUPrice-1
  rm(mx, mlist, matchID, InPlay)
  dfm <- tbl_df(dfm[c('No','Sess','Day','DateUK','Date','Time','Home','Away','Selection','HCap','EUPrice','Stakes','CurScore','Mins','Result','PL','Rebates')])
  dfm <- llply(dfm, function(x){gsub('^\\s{1,}|\\s{1,}$','',x)},.parallel=parallel) %>% data.frame %>% tbl_df %>% mutate_each(funs(as.character)) %>% 
         mutate(No=as.numeric(No),Sess=as.numeric(Sess),DateUK=ymd_hms(DateUK),Date=ymd(Date),Time=hm(Time),HCap=round(as.numeric(HCap),2),
         EUPrice=round(as.numeric(EUPrice),2),Stakes=as.numeric(Stakes),PL=as.numeric(PL),Rebates=round(as.numeric(Rebates),2),
         Return=round(as.numeric(Stakes)+as.numeric(PL),2))
  
  res <- list(datasets=dfm,others=others,corners=corners)
  
  return(res)
}


