arrTeamID <- function(mbase, spboData, match.by='partialMatch', stringdist.method=c('osa','lv','dl','hamming',
                      'lcs','qgram','cosine','jaccard','jw','soundex'), levDist=0.1, parallel=FALSE){
  ## Please choose eihther match.by='partialMatch' or match.by='stringdistList' which is agrep() or 
  ##   stringdist().
  
  ## I just simply apply the `split` function to categorize the teams' name inside `makeList`.
  ## 
  ## STEP 1) Filter the exactly match (or different Capital letter teams' name) teams' name with spboTeamID,
  ##    seperate it to ease the processing time to find the approximate teams' name in section (2).
  ## 
  ## STEP 2) Apply regular expression to filter/seperate the women team, B team or U21 etc prior to partial 
  ##    matching the most approximate teams' name (teams' name others than in STEP 1).
  
  method <- c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw','soundex')
  if(all(stringdist.method %in% method)){
    stop('Please select one or more stringdist.method from above:',cat(method))
  }
  
  source(paste0(getwd(),'/function/makeList.R'))
  ## Since the elements are not much enough but list quit a number, 
  ##   just set parallel=FALSE will be faster few minutes.
  dfm <- makeList(mbase, spboData, levDist=levDist, parallel=parallel)
  
  
  
  df_PM <- llply(seq(nrow(dfm$partialData)), function(i){
                partialMatch(x=dfm$partialData$tmID[i], 
                y=as.character(na.omit(unlist(dfm$partialData[i,4:ncol(dfm$partialData)])),levDist=levDist))
              },.parallel=parallel) %>% rbind_all
  
  ## STEP 3) Option stringdistList() or partialMatch()
  if((match.by!='partialMatch')&(match.by=='stringdistList')){
    ## (a) match.by=='stringdistList'
    ## 
    ## Load the stringdistList() function
    source(paste0(getwd(),'/function/stringdistList.R'))
    #'@ parallel=TRUE ## parallel=FALSE will spend above 30 minutes, but parallel=TRUE will hanging
    df_SD <- llply(seq(nrow(dfm$partialData)), function(i){
      stringdistList(method=method, tmID_A=dfm$partialData$tmID[i], 
                     tmID_B=as.character(na.omit(unlist(dfm$partialData[i,4:ncol(dfm$partialData)]))),
                     levDist=levDist, parallel=parallel)
    },.parallel=parallel) %>% rbind_all
  }
  
  if((match.by=='partialMatch')&(match.by!='stringdistList')){
    ## (b) match.by=='partialMatch'
    ## 
    ## A rogue character in @coneee's data file borked things for me, so let's do a character code 
    ##  conversion first
    ## Load the partialMatch() function
    source(paste0(getwd(),'/function/partialMatch.R'))
    tmID_PM <- llply(seq(dfm$tmID), function(i){
      llply(seq(dfm$tmID[[i]]), function(j){
        if(length(iconv(dfm$tmID[[i]]))>0){
          partialMatch(iconv(dfm$tmID[[i]][[j]]), dfm$spbo[[i]][[j]])}
      })})
    df2 <- partialMatch(dfm$tmID, dfm$spbo)
    df2 <- df2 %>% rename(teamID=raw.x, spboTeamID=raw.y, Matching=pass) %>% tbl_df
    rownames(df1) <- NULL
    
    res <- llply(seq(dfm$tmID),function(i){
              llply(seq(dfm$tmID[[i]]), function(j){
                 if((length(dfm$tmID[[i]][[j]])>0) &(length(dfm$spbo[[i]][[j]])>0)){
                   y <- as.character(sapply(dfm$tmID[[i]][[j]], agrep, dfm$spbo[[i]][[j]],
                                            max.distance=levDist,value=TRUE))
                   stringdistList(method=method, tmID_A=dfm$tmID[[i]][[j]],
                                  tmID_B=y, levDist=levDist, parallel=parallel)
           }}, .parallel=parallel)}, .parallel=parallel)
    df2 <- llply(res,rbind_all) %>% rbind_all
    rm(res)
  }
  
  df1 <- data.frame(c(tmID1A,tmID2A),rep(list(c(spboTM1A,spboTM2A)),ncol(dfm)-1))
  names(df1) <- names(df2)
  dfm <- rbind(df1,df2) %>% data.frame(match=c(rep('duplicate',length(tmID1A)),
         rep('capital',length(tmID2A)),rep('partial',length(tmID2B))),.) %>% tbl_df
  rm(df1,df2)
  
  
  ## ---------------------------------------------------------------------------------
  
}