library(knitr)
library(markdown)
options(warn = -1)

# knitr configuration
opts_knit$set(progress=FALSE) 
opts_chunk$set(echo=TRUE, message=FALSE, tidy=TRUE, comment=NA, 
               fig.path="figure/", fig.keep="high", fig.width=10, fig.height=6, 
               fig.align="center")
knit2html(paste0(getwd(),'/Betting Strategy and Model Validation.Rmd'))
knit2pdf(paste0(getwd(),'/Betting Strategy and Model Validation.Rmd'))
browseURL(paste0(getwd(),'/Betting Strategy and Model Validation.pdf'))
browseURL(paste0(getwd(),'/Betting Strategy and Model Validation.html'))

