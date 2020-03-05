# starting a daily time series in R
# https://stackoverflow.com/questions/33128865/starting-a-daily-time-series-in-r


rm(list=ls())

library(lubridate)
set.seed(42)
minday = as.Date("2001-01-01")
maxday = as.Date("2005-12-31")
dates <- seq(minday, maxday, "days")
dates <- dates[sample(1:length(dates),length(dates)/4)] # create some holes
df <- data.frame(date=sort(dates), val=sin(seq(from=0, to=2*pi, length=length(dates))))

df <- merge(df, data.frame(date=seq(minday, maxday, "days")), all=T)

nts <- ts(df$val, frequency=365, start=c(year(minday), as.numeric(format(minday, "%j"))))
plot(nts)
