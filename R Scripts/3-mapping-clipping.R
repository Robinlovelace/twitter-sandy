## Read CSV
pathTweets <- read.csv("pathTweets.csv",header=T)

###################
##1.1 For US States - Working
###################

## Import state shape files and ensure correct projection
library(rgdal)
states <- readOGR(".", "states")
summary(states)
states <- spTransform(states, CRS("+init=epsg:4326"))
states <- states[-which(grepl("Alask|Haw", as.character(states$STATE_NAME))),]

# Ensure tweet coordinates share projection of state shape files
gT <- SpatialPointsDataFrame(coords=matrix(c(pathTweets$tweet.lon, pathTweets$tweet.lat), 
                                ncol=2),data=pathTweets[, -c(2, 3)], proj4string=CRS("+init=epsg:4326"))

# Create a loop to identify state where each tweet originated & add name to dataframe
pathTweets$state <- NA
pathTweets$stateAbbr <- NA # abbreviated state name

for(i in 1:nrow(states)){
  stateI <- states[which(grepl(states@data$STATE_NAME[i], as.character(states$STATE_NAME))),]
  stateTweets <- gT[stateI,]
  pathTweets$state[ as.integer(row.names(stateTweets)) ] <- as.character(states$STATE_NAME[i])
  pathTweets$stateAbbr[ as.integer(row.names(stateTweets)) ] <- as.character(states$STATE_ABBR[i])
}

#################
##1.2 For US Counties - Working; Source: http://forums.arcgis.com/threads/26330-Where-can-I-find-a-shapefile-with-all-US-counties-and-FIPS-code-for-each
#################

## Import counties shape files and ensure correct projection
library(rgdal)
counties <- readOGR(".", "UScounties")
summary(counties)
data.frame(counties@data$FIPS)
counties <- spTransform(counties, CRS("+init=epsg:4326"))

# Create a loop to identify county where each tweet originated & add name to dataframe
pathTweets$county <- NA
pathTweets$FIPSCode <- NA

for(i in 1:nrow(counties)){
  countyI <- counties[which(grepl(counties@data$FIPS[i], as.character(counties$FIPS))),]
  countyTweets <- gT[countyI,]
  pathTweets$county[ as.integer(row.names(countyTweets)) ] <- as.character(counties$NAME[i])
  pathTweets$FIPSCode[ as.integer(row.names(countyTweets)) ] <- as.character(counties$FIPS[i])
}

# For census tract: https://www.census.gov/geo/maps-data/data/tiger.html

###################
##1.3 For Countries - Not Working; Shape files from Natural Earth: http://www.naturalearthdata.com/downloads/
###################

##Import country shape files and ensure correct projection
library(rgdal)
countries <- readOGR(".", "ne_10m_admin_0_countries")
summary(countries)
countries <- spTransform(countries, CRS("+init=epsg:4326"))
#countries <- countries[-which(grepl("Alask|Haw", as.character(states$STATE_NAME))),]

#Ensure tweet coordinates share projection of state shape files
gT <- SpatialPointsDataFrame(coords=matrix(c(pathTweets$tweet.lon, pathTweets$tweet.lat), 
                                           ncol=2),data=pathTweets, proj4string=CRS("+init=epsg:4326"))

#Create a loop to identify state where each tweet originated & add name to dataframe
pathTweets$continent <- NA #CONTINENT
pathTweets$subregion <- NA #SUBREGION
pathTweets$country <- NA #NAME

for(i in 1:nrow(countries)){
  countryI <- countries[i,]
  countryTweets <- gT[countryI,] # not necessary - this re-writes every time - please delete
  pathTweets$continent[ as.integer(row.names(countryTweets)) ] <- as.character(countries$CONTINENT[i])
  pathTweets$subregion[ as.integer(row.names(countryTweets)) ] <- as.character(countries$SUBREGION[i])
  pathTweets$country[ as.integer(row.names(countryTweets)) ] <- as.character(countries$NAME[i])
}
names(pathTweets)
pathTweets[,31] <- as.factor(pathTweets[,31])
pathTweets[,32] <- as.factor(pathTweets[,32])
pathTweets[,33] <- as.factor(pathTweets[,33])

summary(pathTweets[c("continent", "subregion", "country")])
library(knitr)
kable(summary(pathTweets[c("continent", "subregion", "country")]), row.names=F)
#!!! Nothing past B is labelled in the loop(!?), but doing it seperately works e.g.
uSA <- countries[which(grepl("United States", as.character(countries$NAME))),] 
usTweets <- gT[states,]
pathTweets$country[ as.integer(row.names(usTweets)) ] <- "United States"

#To see list of countries:
data.frame(countries@data$NAME)

#################################################################################
##2.1 Map by location
#################################################################################

library(maps)
library(ggplot2)
library(plyr)
library(RColorBrewer)

# Map tweets by state name
stateSubset <- subset(pathTweets, state == "California") 
plot(states)
points(stateSubset$tweet.lon, stateSubset$tweet.lat, col="green",cex=.6)
# Or plot just the individual state:
plot(states[which(grepl("California", as.character(states$STATE_NAME))),])
points(stateSubset$tweet.lon, stateSubset$tweet.lat, col="green",cex=.6)
qplot(long, lat, data = stateSubset, group = group, fill = log(Freq),geom = "polygon")

# Map tweets by county FIPS code
countySubset <- subset(pathTweets, FIPSCode == "27137") 
plot(counties)
points(countySubset$tweet.lon, countySubset$tweet.lat, col="green",cex=.6)
# Or plot just the individual county:
plot(counties[which(grepl("27137", as.character(counties$FIPS))),])
points(countySubset$tweet.lon, countySubset$tweet.lat, col="green",cex=.6)

############
#2.2 Chloropleth map of States; See https://gist.github.com/cdesante/4252133
############

stateTweetFreq <- data.frame(table(pathTweets$state))
stateTweetFreq <- rename(stateTweetFreq, c("Var1"="region", "Freq"="Freq"))
statesX <- map_data("state")
stateTweetFreq$region <- tolower(stateTweetFreq$region)
choro <- merge(statesX, stateTweetFreq, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]
p <- qplot(long, lat, data = choro, group = group, fill = log(Freq),
        geom = "polygon")
p
p + scale_fill_gradient(low = "#FFFFFF", high = "steelblue")

# Make more attractive plot
theme_map <- theme_classic() + theme(axis.line = element_blank(),
                                     axis.title = element_blank(),
                                     axis.ticks = element_blank(),
                                     axis.text = element_blank()) 
ggplot() +
  geom_polygon(data = choro, aes(long, lat, group = group, fill = Freq/1000)) + 
  geom_path(data = choro, aes(long, lat, group = group)) +
  scale_fill_continuous(low="grey", high="red", name = "Number\nof Tweets\n(1000)") + 
  geom_point(data = usTweets@data, aes(tweet.lon, tweet.lat), alpha = 0.01) +
  theme_map
ggsave("figure/US-overview-rl.png", height=9, width=12)
ggsave("figure/US-overview-rl-lowres.png", height=9, width=12, dpi=50)

############
#2.3 Chloropleth map of counties
############
countyTweetFreq <- data.frame(table(pathTweets$FIPSCode))
countyTweetFreq <- rename(countyTweetFreq, c("Var1"="fips", "Freq"="Freq"))

# Next, need to create a df of map data
countiesX <- map_data('county',region=".",exact=FALSE)
#This is lacking fips codes which ideally should be added as county names
#are not all unique
county.fips <- data.frame(counties@data$NAME,counties@data$FIPS,counties@data$STATE_NAME)
county.fips <- rename(county.fips, c("counties.data.NAME"="subregion","counties.data.STATE_NAME"="region","counties.data.FIPS"="fips"))
county.fips$region <- tolower(county.fips$region)
county.fips$subregion <- tolower(county.fips$subregion)
countiesX <- merge(countiesX, county.fips, by=c("subregion","region"),all.x=TRUE)
#To Sort: 1,400 rows of the countiesX df did not correspond to county.fips so have NA's
#Next, there an issue in that we need to give all counties without any tweet Freq the
#value of zero rather than NA. This requires subseting the rows that have no Freq first.
choroC <- merge(countiesX, countyTweetFreq,by ="fips",all.x=TRUE)
choroX <- merge(countiesX, countyTweetFreq,by ="fips",all.x=FALSE)
choroY <- subset(choroC, !fips %in% choroX$fips)
choroY$Freq <- 0
#Append
choroC <- rbind(choroX,choroY)
choroC <- choroC[order(choroC$order), ]
p <- qplot(long, lat, data = choroC, group = group,fill = log(Freq),geom = "polygon")
p <- p + scale_fill_gradient(low="red", high="green", name = "N. Tweets")
#p <- p + scale_fill_gradient(low = "#FFFFFF", high = "steelblue")
p

#################
##2.4 Plot counties in individual state
#################
stateSubset <- subset(choroC, region=="california")
qplot(long, lat, data = stateSubset, group = group, fill = log(Freq),geom = "polygon")

#Experimental code (ignore):
#countiesX <- merge(countiesX, county.fips, by = "polyname")
#Need to merge based on both state name and county name but the problem is these
#are both in the same column in the county.fips df. Easiest way for me is to
#concatenate to a new column so theres a column that matches.
#countiesX$polyname <- paste(countiesX$region,countiesX$subregion,sep=",")
#countiesX <- merge(countiesX, county.fips, by = "polyname")
#choroC <- merge(countiesX, countyTweetFreq,by ="fips")
#choroC <- choro[order(choroC$order), ]
#library(maptools)
#testfile <- '/Users/eel2nr/Documents/1 Sandy/7 Repos/twitter-sandy/UScounties.shp'
#test <- readShapePoly(testfile)
#See https://stat.ethz.ch/pipermail/r-sig-geo/2010-June/008520.html


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









##Import country shape files and ensure correct projection
library(rgdal)
countries <- readOGR(".", "ne_10m_admin_0_countries")
summary(countries)
countries <- spTransform(countries, CRS("+init=epsg:4326"))
#countries <- countries[-which(grepl("Alask|Haw", as.character(states$STATE_NAME))),]

#Ensure tweet coordinates share projection of state shape files
gT <- SpatialPointsDataFrame(coords=matrix(c(pathTweets$tweet.lon, pathTweets$tweet.lat), 
                                           ncol=2),data=pathTweets[, -c(2, 3)], proj4string=CRS("+init=epsg:4326"))

#Countries affected
cA <- countries[which(grepl("Ireland", as.character(countries$NAME))),] 
plot(cA)

#subset all tweets in Ireland
iT <- gT[cA,]
points(iT,col="green")


write.csv(pathTweets, "pathTweets.csv")
