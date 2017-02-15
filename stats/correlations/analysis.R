library(ggplot2)

dcrime <- read.csv("fl_crime.csv")
dhousing <- read.csv("florida-housing-permits.csv")
dunemp <- read.csv("florida-unemployment.csv")
dgdp <- read.csv("gdp.csv")

dhousing <- data.frame(Year=as.numeric(substring(colnames(dhousing), 2, 5)), Permits=as.numeric(dhousing[1,]))
dunemp <- data.frame(Year=as.numeric(substring(colnames(dunemp), 2, 5)), Unemp=as.numeric(dunemp[1,]))

d <- merge(merge(merge(dcrime, dhousing, by="Year"), dunemp, by="Year"), dgdp, by="Year")