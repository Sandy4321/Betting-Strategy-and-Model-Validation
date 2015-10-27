## Web scrapping on leagues and teams
## Loading the packages
if(!'devtools' %in% installed.packages()){
  install.packages('devtools')}
if(!'BBmisc' %in% installed.packages()){
  install.packages('BBmisc')}
if(!'RSeleniumUtilities' %in% installed.packages()){
   ## https://github.com/greenore/RSeleniumUtilities
   source("https://rawgit.com/greenore/initR/master/init.R")
   packagesGithub("RSeleniumUtilities", repo_name="greenore")}

suppressPackageStartupMessages(library('BBmisc'))
suppressPackageStartupMessages(lib(c('rJava','RSelenium','rvest','XML','RCurl','RSeleniumUtilities')))

## Setup Java path
#'@ Sys.getenv('JAVA_HOME')
#'@ Sys.setenv(JAVA_HOME=Sys.getenv('JAVA_HOME'))
Sys.setenv(PHANTOM_JS='phantomjs-1.9.8-linux-x86_64')

Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-8-oracle/jre')
#'@ Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-8-oracle-amd64/jre/lib/amd64/server')
Sys.setenv(LD_LIBRARY_PATH='/usr/lib/jvm/java-8-oracle/jre/server: open -a rstudio')
#'@ Sys.setenv(JAVA_HOME='/usr/lib/jvm/java-8-openjdk/jre')
#'@ Sys.getenv('LD_LIBRARY_PATH')
#'@ Sys.getenv()
#'@ packageDescription('rJava')

## ==========================================================
Sys.setenv(java_path='/usr/lib/jvm/java-8-openjdk/jre')
RSeleniumUtilities::checkSelenium()


## ==========================================================
## Scrape the leagues and also overrounds which provides by a sportsbookmaker named Firm B
## Besides, need to scrap the final-scores / half-time scores / result of soccer matches
teamID <- sort(unique(c(as.character(mbase$Home), as.character(mbase$Away))))
dateID <- sort(unique(mbase$Date)); spboDate <- gsub('-','',dateID)
#'@ lnk <- paste0('http://data2.7m.cn/result_data/default_en.shtml?date=',dateID)
#'@ lnk <- paste0('http://www8.spbo.com/history.plex?day=',spboDate,'&l=en')
lnk <- 'http://data.nowgoal.com/history/handicap.htm'

## web scrapping
## https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-headless.html
## https://github.com/eugeneware/phantomjs-bin
pJS <- phantom(paste0(getwd(),'/phantomjs'))
Sys.sleep(5) # give the binary a moment
webDr <- remoteDriver(browserName = 'phantomjs')
webDr$open(silent=TRUE)
webDr$navigate(lnk) ## for loop

## //*[@id="matchdate"]
sess <- html_session(lnk)
f0 <- sess %>% html_form
f1 <- set_values(f0[[2]], 'matchdate'=dateID[1],
                 'companyid1'=list(c(3,8,4,12,1,23,24,17,31,14,35,22)))
s <- submit_form(sess, f1)
f1 <- set_values(f0[[1]],'button'=FALSE,'button'=TRUE,'button'=TRUE)
f1$url <- lnk
s <- submit_form(sess, f1)


sess <- html_session(lnk) %>% 
  CapVal <- sess %>% html_form %>% .[[1]]
form <- sess %>% html_form %>% .[[2]] %>% set_values('button'='********', 'Password'='********', 'CaptchaValue'=CapVal)
form
iBET <- sess %>>% submit_form(form)

## ========================================================================
webElem <- webDr$findElement(using = 'xpath', value = '//*[@id="main"]/ul/li[2]/a')
webElem$clickElement()
webElem <- webDr$findElement(using = 'xpath', value = '//*[@id="button1"]')
webElem$clickElement()
webElem <- webDr$findElement(using = 'xpath', value = '//*[@id="button0"]')
webElem$clickElement()
web <- webDr$getPageSource()[[1]]
#'@ tab <- html_session(web) %>% html_nodes('a') ##doesn't work
tab <- readHTMLTable(htmlParse(web), header=TRUE)

## =============================================================
lnk <- 'http://data.nowgoal.com/history/handicap.htm'
html_session(lnk) %>% html_nodes('div.odds') %>% html_table


## =============================================================
RSelenium::checkForServer()
RSelenium::startServer()
eCap <- list(phantomjs.binary.path = '/home/rstudio/phantomjs')
webDr <- remoteDriver(browserName = 'phantomjs', extraCapabilities = eCap)





