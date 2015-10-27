partialMatch <- function(x, y, levDist=0.1){
  ## http://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/
  ## (1) Here's where the algorithm starts...
  ## I'm going to generate a signature from country names to reduce some of the minor differences between strings
  ## In this case, convert all characters to lower case, sort the words alphabetically, and then concatenate them with no spaces.
  ## So for example, United Kingdom would become kingdomunited
  ## We might also remove stopwords such as 'the' and 'of'.
  
  ## ---------------------------------------------------------------------------------------------  
  signature <- function(x){
    sign <- paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
    return(sign)
  }
  
  ## ---------------------------------------------------------------------------------------------
  xx <- data.frame(sign=sapply(x, signature),row.names=NULL)
  yy <- data.frame(sign=sapply(y, signature),row.names=NULL)
  
  xx$raw <- x
  yy$raw <- y
  xx <- subset(xx,subset=(sign!=''))
  xy <- merge(xx,yy,by='sign',all=T)
  
  matched <- subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass <- 'Duplicate'
  todo <- subset(xy,subset=(is.na(raw.y)),select=c(sign,raw.x))
  colnames(todo) <- c('sign','raw')
  todo$partials <- as.character(sapply(todo$sign, agrep, yy$sign,max.distance = levDist,value=TRUE))
  todo <- merge(todo,yy,by.x='partials',by.y='sign')
  
  partial.matched <- subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c('sign','raw.x','raw.y'))
  partial.matched$pass <- 'Partial'
  matched <- rbind(matched,partial.matched)
  un.matched <- subset(todo,subset=(is.na(raw.x)),select=c('sign','raw.x','raw.y'))
  
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched <- rbind(matched,un.matched)
  }
  matched <- subset(matched,select=c("raw.x","raw.y","pass"))
  dimnames(matches) <- list(NULL,c('tmID','spboID','Match'))
  
  return(matched)
}