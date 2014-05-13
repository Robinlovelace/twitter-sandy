########################################################################################
##STEP ONE: Formatting & Filtering the Data for Use in R
########################################################################################

##Set the working directory where the files will be read from & saved to
setwd("~/Documents/1 Sandy/7 Repos/twitter-sandy/")

#### HURRICANE PATH ####################################################################

library(lubridate)
##Read the hurricane path file which extrapolates the NOAA Best Track data
hurricanePath <- read.csv("hurricanePath.txt",header=T)
##Convert date and time to POSIXct format readable by R (GMT is UTC)
hurricanePath$utcTime <- as.POSIXct(as.character(hurricanePath$utc),"%Y-%m-%d %H:%M",tz="GMT")
##Convert to Eastern Time using the format function (EDT UTC-4hrs; EST UTC-5hrs during winter)
hurricanePath$easternTime <- format(hurricanePath$utcTime,tz="America/New_York", usetz=TRUE)

# write.csv(file="hurricanePath.csv",x=hurricanePath, row.names=FALSE) # save

#### TWEETS ############################################################################

##Read the geo-tagged tweets file into R
tweets <- read.csv("allSandyGeo.csv",header=T)
##Convert date and time to POSIXct & POSIXlt format readable by R (GMT is UTC)
tweets$utcTime <- as.POSIXct(as.character(tweets$utc),"%Y-%m-%d %H:%M",tz="GMT")
##Convert to Eastern Time using the format function (EDT UTC-4hrs; EST UTC-5hrs during winter)
tweets$easternTime <- format(tweets$utcTime,tz="America/New_York", usetz=FALSE)
##Add the R-formatted date and time to the dataframe
tweets <- data.frame(tweets,utcTime,easternTime)
##Remove superfluous columns
tweets$utc <- NULL
tweets$postedTime <- NULL
##Add further date/time details for EDT time
dayOfYear <- yday(tweets[,17])
monthName <- month(tweets[,17],label=TRUE)
dayName <- wday(tweets[,17],label=TRUE,abbr=TRUE)
##Add these to the dataframe
tweets <- data.frame(tweets,dayOfYear,monthName,dayName)

##Identifying false positives in the tweets
##1. Sender user names containing 'sandy' 
nameSandy <- which(grepl("sandy", tweets$actor.preferredUsername, ignore.case = T))
##2. Tweets sent to anyone whose name contains 'sandy'
nameLinkSandy <- which(grepl("sandy", tweets$inReplyTo.link, ignore.case = T))
##3. Sandy not being a complete word
##Often the "sandy" string is part of a larger text string that does not refer to the
##hurricane ("hurricanesandy" is an exception with 18313 mentions). We can identify
##these false positives using regular expressions. Sandy followed by an alphanumeric:
sSandy <- which(grepl("sandy[[[:alnum:]]", tweets$body, ignore.case = T))
#Filter 'sandy' tags with alphanumeric prefix excluding e
sSandyB <- which(grepl("[abcdfghijklmnopqrstuvwxyx0123456789]sandy",tweets$body,ignore.case=T))

##A look at these tweets filtered above shows that few appear directly related to the hurricane
head(tweets$body[nameSandy], 20)
head(tweets$body[nameLinkSandy], 20)
head(tweets$body[sSandy], 20)
head(tweets$body[sSandyB], 20)

##They respectively make up 0.03%, 2.3%, 13% and 5% of the tweets
length(nameSandy)/nrow(tweets)
length(nameLinkSandy)/nrow(tweets)
length(sSandy)/nrow(tweets)
length(sSandyB)/nrow(tweets)

##Exclude the false positives from the dataset
filteredTweets <- tweets[-sSandy, ]
filteredTweets <- filteredTweets[-sSandyB, ]
filteredTweets <- filteredTweets[-nameLinkSandy, ]
filteredTweets <- filteredTweets[-nameSandy, ]

##False positives make up around 20% of the original geo-tagged dataset
1 - nrow(filteredTweets)/nrow(tweets)

write.csv(file="filteredTweets.csv",x=filteredTweets, row.names=FALSE)

filteredTweets <- read.csv("filteredTweets.csv",header=T)
hurricanePath <- read.csv("hurricanePath.csv", header=T)

##Merge the hurricane and path df
summary(filteredTweets$utcTime)[1:10]
class(filteredTweets$utcTime)
summary(hurricanePath$utcTime)[1:10]
summary(filteredTweets$utcTime %in% hurricanePath$utcTime)
filteredTweets$utc2 <- as.character(filteredTweets$utcTime)
hurricanePath$utc2 <- as.character(hurricanePath$utcTime)
# filteredTweets$utc2 <- strtrim(filteredTweets$utc2, 16) # trim off seconds if needed
summary(filteredTweets$utc2 %in% hurricanePath$utc2)

pathTweets <- merge(filteredTweets, hurricanePath, by="utc2", all.x=T)
write.csv(pathTweets, "pathTweets.csv")
head(pathTweets)

########################################################################################
##NOTES, GUIDANCE & REFERENCES:
##
##blog.revolutionanalytics.com/2009/06/converting-time-zones.html [For converting tz]
##http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones [For tz codes]
##
##The tweet file uses the following format: 2012-10-25T00:00:12.000Z. It has been
##reformated in Excel before being read into R to make it easier to use. The Excel
##function used is: =CONCATENATE(LEFT(S2,10)&" "&MID(S2,12,8)). To copy it for entire
##column, double click on the bottom right corner of cell when highlighted.
##
##To convert date and time to POSIXlt format instead:
##utcTime <- strptime(as.character(hurricanePath$utc),"%Y-%m-%d %H:%M",tz="GMT")
########################################################################################