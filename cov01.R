# Corona Virus Time Series analysis (to see how the infection trend will be subsided, hopefully.)


rm(list=ls())
setwd("~/R/Time Series Analysis/Time Series Analysis")


library(lattice)
library(ggplot2)

# latticeExtra must be loaded after ggplot2 to prevent masking of `layer`
library(latticeExtra)
library(RColorBrewer)
# lattice and latticeExtra configuration
myTheme <- custom.theme.2(
  pch=19, cex=0.7, region=rev(brewer.pal(9, 'YlOrRd')),
  symbol=brewer.pal(n=8, name="Dark2"))
myTheme$strip.background$col = myTheme$strip.shingle$col =
  myTheme$strip.border$col = 'transparent'

myArgs <- list(
  as.table=TRUE, between=list(x=0.5, y=0.2),
  xscale.components = function(...)
    modifyList(xscale.components.default(...), list(top=FALSE)),
  yscale.components = function(...)
    modifyList(yscale.components.default(...), list(right=FALSE)))

lattice.options(default.theme=myTheme, default.args=modifyList(
  lattice.options()$default.args, myArgs))


library(zoo) 


cof <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
death <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recov  <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")



library (readr)

url1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
url2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
url3 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"  
  
cof1<-read_csv(url(url1))
death1<-read_csv(url(url2))
recov1<-read_csv(url(url3))


cofts <- tidyr::gather(cof1, "Province/State","n", 5:46)
str(cofts)
