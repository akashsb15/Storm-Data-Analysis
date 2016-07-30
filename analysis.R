## Set Working Directory
setwd("C:/Users/Admin/Documents/Coursera/R/data/Course5/Week4/Storm-Data-Analysis")

## Download Data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
              destfile = "stormData.csv.bz2", mode = "wb")

## Load Data
myData <- read.csv(bzfile("stormData.csv.bz2"))


## Part One, Damage to Health
reqData <- myData[,c("EVTYPE", "FATALITIES", "INJURIES")]

library(ggplot2)
library(dplyr)

## Group by EVTYPE
groupByEvt <- group_by(reqData, EVTYPE)
groupByEvt <- summarize(groupByEvt, sum(FATALITIES), sum(INJURIES))

## Extract information about fatalites
groupByEvtF <- groupByEvt[order(-groupByEvt$`sum(FATALITIES)`),]
groupByEvtF <- head(groupByEvtF, 10)[,c(1,2)]

## Plot
g <- ggplot(groupByEvtF, aes(EVTYPE, `sum(FATALITIES)`))
g <- g + geom_bar(stat = "identity") 
g <- g + theme(axis.text.x = element_text(angle=90, vjust=1, size = 10)) 
g <- g + labs(x = "EVTYPE - Event Type", y = "Number of Fatalities", title = "10 Most Dangerous Natural Events (Fatality Count)")
g

## Extract information about injuries
groupByEvtI <- groupByEvt[order(-groupByEvt$`sum(INJURIES)`),]
groupByEvtI <- head(groupByEvtI, 10)[,c(1,3)]


## Plot
g <- ggplot(groupByEvtI, aes(EVTYPE, `sum(INJURIES)`))
g <- g + geom_bar(stat = "identity") 
g <- g + theme(axis.text.x = element_text(angle=90, vjust=1, size = 10)) 
g <- g + labs(x = "EVTYPE - Event Type", y = "Number of Injuries", title = "10 Most Dangerous Natural Events (Injury Count)")
g


## Extract Data for economic losses
reqData <- myData[,c("EVTYPE","PROPDMG","PROPDMGEXP")]
groupByEvt <- group_by(reqData, EVTYPE)

## Create a new feature that contains the total damage for an event
groupByEvt <- mutate(groupByEvt, TOTDMG = ifelse(PROPDMGEXP == "M", PROPDMG*10^6, PROPDMG*10^3))
groupByEvt <- summarize(groupByEvt, sum(TOTDMG))

## Extract top 10 events
groupByEvt <- groupByEvt[order(-groupByEvt$`sum(TOTDMG)`),]
groupByEvt <- head(groupByEvt, 10)

## Plot
g <- ggplot(groupByEvt, aes(EVTYPE, `sum(TOTDMG)`))
g <- g + geom_bar(stat = "identity") 
g <- g + theme(axis.text.x = element_text(angle=90, vjust=1, size = 10)) 
g <- g + labs(x = "EVTYPE - Event Type", y = "Expense in Dollars", title = "10 Most Expensive Natural Events")
g
