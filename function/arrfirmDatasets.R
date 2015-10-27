arrfirmDatasets <- function(dfList, lProfile=c(AH=0.10,OU=0.12)){
  ## Here I take the majority leagues setting profile which are "league-10-12"
  ## fMYPriceB = Back with vigorish price; fMYPriceL = Lay with vigorish price
  ## Here we term as Fair Odds
  
  require('stringr',quietly=TRUE)
  require('doParallel',quietly=TRUE)
  
  df <- dfList$datasets
  if((is.error(df))|(is.null(df))){
    stop('Please apply readfirmDatasets() to read the datasets of Sports Consultancy Firm prior to reprocess!')
  }else{
    df$Picked <- sapply(strsplit(as.character(df$Selection),' - '),function(x) x[length(x)])
    df$AHOU <- factor(ifelse(df$Picked=='over'|df$Picked=='under','OU','AH'))
    lProfile <- lProfile ## League setting profile
    
    ## Convert backed HKPrice to MYPrice in order to calculate the Layed Price
    df$fMYPriceB <- ifelse(df$HKPrice>1, -1/(df$HKPrice), df$HKPrice)
    df$fMYPriceL <- ifelse(df$fMYPriceB<0 & df$AHOU=='AH', abs(df$fMYPriceB+lProfile[1]),
                    ifelse(df$fMYPriceB<=1 & df$fMYPriceB>lProfile[1] & df$AHOU=='AH', 2-df$fMYPriceB-lProfile[1],
                    ifelse(df$fMYPriceB<=1 & df$fMYPriceB>lProfile[2] & df$AHOU=='OU', 2-df$fMYPriceB-lProfile[2],
                    ifelse(df$fMYPriceB<0 & df$AHOU=='OU', abs(df$fMYPriceB+lProfile[2]), 0))))
    df$fMYPriceL <- ifelse(df$fMYPriceL>1, -1/(df$fMYPriceL), df$fMYPriceL)
  
    ## A price listing which summarise the min to mx of Price backed by firm A on agency A
    mPrice <- unique(df[c('EUPrice','HKPrice','fMYPriceB')])
    mPrice <- mPrice[order(mPrice$EUPrice, decreasing=FALSE),]
    df$pHKRange <- cut(df$HKPrice, seq(floor(min(mPrice$HKPrice)), ceiling(max(mPrice$HKPrice)), 0.1))
    df$fHKPriceL <- ifelse(df$fMYPriceL<0, -1/df$fMYPriceL, df$fMYPriceL)
    
    ## Categorize the selection by either 'favorite' or 'underdog', 'under' or 'over'
    df$Picked <- factor(ifelse(as.character(df$AHOU)=='AH' & as.numeric(df$HCap)>0,'underdog',
                        ifelse(as.character(df$AHOU)=='AH' & as.numeric(df$HCap)<0,'favorite',
                        ifelse(as.character(df$AHOU)=='AH' & as.numeric(df$HCap)==0 &
                               as.numeric(df$HKPrice)< as.numeric(df$fHKPriceL),'favorite',
                        ifelse(as.character(df$AHOU)=='AH' & as.numeric(df$HCap)==0 &
                               as.numeric(df$HKPrice)> as.numeric(df$fHKPriceL),'underdog',
                        ifelse(as.character(df$AHOU)=='AH' & as.numeric(df$HCap)==0 &
                               as.numeric(df$HKPrice)==as.numeric(df$fHKPriceL),'favorite', df$Picked))))))
                               
    ## Add an InPlay data range
    ## 1. Cut the time range to be 5 mins interval time
    ##
    df$InPlay <- factor(ifelse(str_detect(df$Mins,'ET'),'ET',ifelse(str_detect(df$Mins,'No'),'No', 'FT')))
    df$Mins2 <- gsub('ET','', as.character(df$Mins))
    df$Mins2 <- factor(ifelse(str_detect(df$Mins2,'HT'),'HT',ifelse(str_detect(df$Mins2,'FT'),'FT',
                       ifelse(str_detect(df$Mins2,'No'),'No',gsub('[^0-9]','', as.character(df$Mins2))))))
    df$InPlay2 <- factor(ifelse(df$Mins2=='HT'|df$Mins2=='FT'|df$Mins2==0,'Break', as.character(df$InPlay)))
    
    df <- ldply(split(df,df$InPlay2), function(x) data.frame(x, ipRange=suppressWarnings(cut(as.numeric(as.character(x$Mins2)),breaks=seq(0,90,5)))))
    
    ##
    ## 2. Set the current handicap right before scoring a goal
    ##
    SC <- ldply(strsplit(ifelse(df$CurScore=='No','0-0', as.character(df$CurScore)),'-'))
    SC <- data.frame(lapply(SC, as.numeric))
    names(SC) <- c('HG','AG')
    df$HG <- SC$HG; df$AG <- SC$AG; rm(SC,mPrice)
      
    ## First half, Full-time or Extra time
    df$FHFTET <- factor(ifelse(str_detect(df$Home,'1st Half'),'FH', ifelse(str_detect(df$Mins,'ET'),'ET','FT')))
    df$Picked2 <- factor(ifelse(as.character(df$Selection)==as.character(df$Home),'home',
                         ifelse(as.character(df$Selection)==as.character(df$Away),'away', as.character(df$Picked))))
    df$ipHCap <- ifelse(df$Picked2=='over', (df$HG + df$AG) - df$HCap,
                 ifelse(df$Picked2=='under', df$HCap - (df$HG + df$AG),
                 ifelse(df$Picked=='favorite' & df$Picked2=='home', (df$HG + df$HCap) - df$AG,
                 ifelse(df$Picked=='favorite' & df$Picked2=='away', (df$AG + df$HCap) - df$HG,
                 ifelse(df$Picked=='underdog' & df$Picked2=='home', df$AG - (df$HG + df$HCap),
                 ifelse(df$Picked=='underdog' & df$Picked2=='away', df$HG - (df$AG + df$HCap), NA))))))
    #'@ df[c('Picked','Picked2','ipRange','FHFTET','AHOU','HCap','HKPrice','CurScore','ipHCap')]
    dfList$datasets <- df
    return(dfList)
  }
}



