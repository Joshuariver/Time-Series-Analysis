# coronavirus package


rm(list=ls())


library(coronavirus)

data("coronavirus")
str(coronavirus)

cor <- coronavirus

cor$type <- as.factor(cor$type)
str(cor)
