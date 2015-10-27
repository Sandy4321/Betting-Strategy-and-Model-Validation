readSPBO2 <- function(dateID=dateID, parallel=FALSE){
  ## Loading the packages
  suppressPackageStartupMessages(require('plyr',quietly=TRUE))
  suppressPackageStartupMessages(require('dplyr',quietly=TRUE))
  suppressPackageStartupMessages(require('stringr',quietly=TRUE))
  
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
  
  dfm <- rbind_all(llply(as.list(dateID), function(x)
         data.frame(read.csv(paste0(getwd(),'/datasets/livescore/',x,'.csv'),header=TRUE,sep=',')),
         .parallel=parallel))
  
  ## change all columns' class at once
  ## http://stackoverflow.com/questions/27668266/dplyr-change-many-data-types#_=_
  dfm <- dfm %>% mutate(No=seq(nrow(.)), X=as.numeric(X), matchID=factor(matchID), LeagueColor=factor(LeagueColor),
                        League=factor(League), DateUK=format(as.POSIXct(strptime(Date,format='%Y/%m/%d %H:%M',
                        tz='Asia/Hong_Kong'),tz='Asia/Hong_Kong'), tz='Europe/London',usetz=TRUE),
                        Time=factor(str_split_fixed(Date,' ',2)[,02]), Date=as.Date(str_split_fixed(Date,' ',2)[,01]),
                        Finished=as.numeric(Finished), Home=factor(Home), Away=factor(Away),
                        FTHG=as.numeric(FTHG), FTAG=as.numeric(FTAG), HTHG=as.numeric(HTHG),HTAG=as.numeric(HTAG),
                        H.Card=as.numeric(H.Card), A.Card=as.numeric(A.Card), HT.matchID=factor(HT.matchID),
                        HT.graph1=as.numeric(HT.graph1), HT.graph2=as.numeric(HT.graph2))
  
  dfm <- tbl_df(data.frame(dfm[c('No','X','matchID','LeagueColor','League','DateUK','Date',
        'Time','Finished','Home','Away','FTHG','FTAG','HTHG','HTAG','H.Card','A.Card','HT.matchID',
        'HT.graph1','HT.graph2')]))
  
  return(dfm)
}


