library('rmarkdown')

# knitr configuration
opts_knit$set(progress=FALSE) 
opts_chunk$set(echo=TRUE, message=FALSE, tidy=TRUE, comment=NA, 
               fig.path="figure/", fig.keep="high", fig.width=10, fig.height=6, 
               fig.align="center") 

rmarkdown::render('Natural Language Analysis.Rmd','all')
browseURL('Natural Language Analysis.html')
browseURL('Natural_Language_Analysis.html')


