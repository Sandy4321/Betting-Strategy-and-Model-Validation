arrTeamID <- function(mbase, spboData, parallel=TRUE){
  suppressPackageStartupMessages(require('stringdist', quietly=TRUE))
  suppressPackageStartupMessages(require('doParallel', quietly=TRUE))
  suppressPackageStartupMessages(require('plyr', quietly=TRUE))
  suppressPackageStartupMessages(require('dplyr', quietly=TRUE))
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
  
  ext <- c('2','3','4','5','6','7','II','III','IV','V','VI','VII','B','C','D',paste0('U',seq(15,23)))
  
  
  ## ---------------------------------------------------------------------------------
  ## http://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/
  ## (1) Here's where the algorithm starts...
  ## I'm going to generate a signature from country names to reduce some of the minor differences between strings
  ## In this case, convert all characters to lower case, sort the words alphabetically, and then concatenate them with no spaces.
  ## So for example, United Kingdom would become kingdomunited
  ## We might also remove stopwords such as 'the' and 'of'.
  signature <- function(x){
    sig <- paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
    return(sig)
  }

  partialMatch=function(x,y,levDist=0.1){
    xx <- data.frame(sig=sapply(x, signature),row.names=NULL)
    yy <- data.frame(sig=sapply(y, signature),row.names=NULL)
    xx$raw <- x
    yy$raw <- y
    xx <- subset(xx,subset=(sig!=''))
    xy <- merge(xx,yy,by='sig',all=T)
    matched <- subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
    matched$pass <- 'Duplicate'
    todo <- subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
    colnames(todo) <- c('sig','raw')
    todo$partials <- as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
    todo <- merge(todo,yy,by.x='partials',by.y='sig')
    partial.matched <- subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c('sig','raw.x','raw.y'))
    partial.matched$pass <- 'Partial'
    matched <- rbind(matched,partial.matched)
    un.matched <- subset(todo,subset=(is.na(raw.x)),select=c('sig','raw.x','raw.y'))
    
    if (nrow(un.matched)>0){
      un.matched$pass="Unmatched"
      matched <- rbind(matched,un.matched)
    }
  matched <- subset(matched,select=c("raw.x","raw.y","pass"))
  
  return(matched)
  }
  
  ## ---------------------------------------------------------------------------------
  #A rogue character in @coneee's data file borked things for me, so let's do a character code conversion first
  tmID <- iconv(tmID)
  df1 <- partialMatch(tmID,spboTeamID)
  df1 <- df1 %>% rename(teamID=raw.x, spboTeamID=raw.y, Matching=pass) %>% tbl_df
  rownames(df1) <- NULL
  
  tmID2 <- tmID[!tmID %in% df1$teamID]
  tmp.spbo <- spboTeamID[!spboTeamID %in% df1$spboTeamID]
  
  ## (2) Apply stringdist() to match the most approximate matching team names
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
  ## http://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets
  ext <- c('2','3','4','5','6','7','II','III','IV','V','VI','VII','B','C','D',paste0('U',seq(15,23)))
  dm <- expand.grid(tmID2,tmp.spbo) # Distance matrix in long form
  dm[regexpr('.{1,3}$',dm$Var1) %in% ext,]
  
  
  
  names(dm) <- c('teamID','spboTeamID')
  dm$dist <- stringdist(dm$teamID,dm$spboTeamID, method='jw') # String edit distance (use your favorite function here)
  
  # Greedy assignment heuristic (Your favorite heuristic here)
  greedyAssign <- function(a,b,dm){
    x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable, 
    # 1 for already assigned, -1 for unassigned and unassignable
    while(any(x==0)){
      min_dm <- min(dm[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
      a_sel <- a[dm==min_dm & x==0][1] 
      b_sel <- b[dm==min_dm & a == a_sel & x==0][1] 
      x[a==a_sel & b == b_sel] <- 1
      x[x==0 & (a==a_sel|b==b_sel)] <- -1
    }
    cbind(a=a[x==1],b=b[x==1],dm=dm[x==1])
  }
  data.frame(greedyAssign(as.character(dm$teamID),as.character(dm$spboTeamID),dm$dist))
  
  ## ---------------------------------------------------------------------------------
  
}