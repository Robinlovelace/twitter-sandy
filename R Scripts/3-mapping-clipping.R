##Read CSV
pathTweets <- read.csv("pathTweets.csv",header=T)

##Import state shape files and ensure correct projection
library(rgdal)
states <- readOGR(".", "states")
summary(states)
states <- spTransform(states, CRS("+init=epsg:4326"))
states <- states[-which(grepl("Alask|Haw", as.character(states$STATE_NAME))),]

#Ensure tweet coordinates share projection of state shape files
gT <- SpatialPointsDataFrame(coords=matrix(c(pathTweets$tweet.lon, pathTweets$tweet.lat), 
                                ncol=2),data=pathTweets[, -c(2, 3)], proj4string=CRS("+init=epsg:4326"))

#Create a loop to identify state where each tweet originated & add name to dataframe
pathTweets$state <- NA
pathTweets$stateAbbr <- NA

for(i in 1:nrow(states)){
  stateI <- states[which(grepl(states@data$STATE_NAME[i], as.character(states$STATE_NAME))),]
  stateTweets <- gT[stateI,]
  pathTweets$state[ as.integer(row.names(stateTweets)) ] <- as.character(states$STATE_NAME[i])
  pathTweets$stateAbbr[ as.integer(row.names(stateTweets)) ] <- as.character(states$STATE_ABBR[i])
}

#Map tweets by state name
stateSubset <- subset(pathTweets, state == "California") 
plot(states)
points(stateSubset$tweet.lon, stateSubset$tweet.lat, col="green",cex=.6)

#Plot US
library(maps)
library(ggplot2)
stateTweetFreq <- data.frame(table(pathTweets$state))
library(plyr)
stateTweetFreq <- rename(stateTweetFreq, c("Var1"="region", "Freq"="Freq"))
statesX <- map_data("state")
stateTweetFreq$region <- tolower(stateTweetFreq$region)
choro <- merge(statesX, stateTweetFreq, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
qplot(long, lat, data = choro, group = group, fill = log(Freq),
        geom = "polygon")




# check plotting works
#plot(gT[1:100,])
# the data frame inside gT
#head(gT@data)
#head(gT$actor.link)
# check the states and tweets are in the same projectionn
#plot(states)
#points(gT)

# states affected
sA <- states[which(grepl("New Jersey", as.character(states$STATE_NAME))),] 
plot(sA)

#subset all tweets in NY
njT <- gT[sA,]
points(njT,col="green")
#head(row.names(njT))
gT$State <- NA
gT$State[ as.integer(row.names(njT)) ] <- "NJ"
pathTweets$state <- gT$State

# check the subset
#head(gT@data[ row.names(njT), ], 50 ) # seems ok
#length(grep("NJ", gT$body[ gT$State == "NJ"])) / length(which(gT$State == "NJ")) 
#length(grep("NJ", gT$body)) / nrow(gT)



