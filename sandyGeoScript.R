########################################################################################
##STEP TWO: Calculate distance & bearing between hurricane & tweets at moment of origin
########################################################################################

##Read CSV
pathTweets <- read.csv("pathTweets.csv",header=T)

##Load package
library(geosphere)

##Calculate distance between hurricane and each tweet. Method calculates shortest
##distance between two points (i.e., the ’great-circle-distance’ or ’as the crow flies’),
##according to the ’Vincenty (ellipsoid)’ method.
distance = NULL
for (i in 1:nrow(pathTweets)) {
  distance[i] =
    distVincentyEllipsoid(c(pathTweets[i,3],pathTweets[i,2]),c(pathTweets[i,23],pathTweets[i,22]), a=6378137, b=6356752.3142, f=1/298.257223563)
}
km <- distance*0.001
pathTweets <- cbind(pathTweets, km)
write.csv(file="pathTweets.csv",x=pathTweets, row.names=FALSE)

##Calculate initial bearing to tweet
bearingToTweet = NULL
for (i in 1:129936) {
  bearingToTweet[i] =
    bearing(c(pathTweets[i,3],pathTweets[i,2]),c(pathTweets[i,23],pathTweets[i,2]))
}
#x <- lapply(bearingToTweet, function(x) { x[1:162343] }) #Adds NA's remaining rows
pathTweets$bearing <- NA
pathTweets$bearing[1:129936] <- bearingToTweet
write.csv(file="pathTweets.csv",x=pathTweets, row.names=FALSE)

##Calculate initial bearing of hurricane
hurricanePathB <-read.csv("hurricanePathB.csv",header=T)
pathTweets <- merge(pathTweets, hurricanePathB)
write.csv(file="pathTweets.csv",x=pathTweets, row.names=FALSE)
bearingOfHurricane = NULL
for (i in 1:129936) {
  bearingOfHurricane[i] =
    bearing(c(pathTweets[i,4],pathTweets[i,3]),c(pathTweets[i,26],pathTweets[i,25]))
}
write.csv(file="bearingOfHurricane.csv",x=bearingOfHurricane, row.names=FALSE)
##Manually added NAs in Excel
bearingOfHurricane <- read.csv("bearingOfHurricane.csv",header=T)
pathTweets <- cbind(pathTweets, bearingOfHurricane)
write.csv(file="pathTweets.csv",x=pathTweets, row.names=FALSE)

##Save dataframe as CSV called pathTweets
write.csv(file="pathTweets.csv",x=pathTweets, row.names=FALSE)


########################################################################################
##NOTES, GUIDANCE & REFERENCES:
##
##http://cran.r-project.org/web/packages/geosphere/geosphere.pdf
########################################################################################

########################################################################################
##STEP THREE: Basic Visualisations
########################################################################################

##Read CSV
pathTweets <- read.csv("pathTweets.csv",header=T)

##Load Package
library(ggplot2)
library(grid) ##Required for the 'unit' function when adjusting margins
library(lubridate)

##Tweets by Hour as a histogram
y<-as.POSIXct(pathTweets$easternTime)
p<-qplot(y,binwidth=60*60,fill=I('steelblue'),xlab="Day (East Coast)",ylab="Tweets Per Hour")
p<-p+scale_x_datetime(major="1 day",
                      minor="1 day",
                      format="%e/%m/%Y",
                      limits=c(as.POSIXct('2012/10/24'),
                               as.POSIXct('2012/11/02')))
print(p)

##Tweets by hour as a frequency table
##By day
table(pathTweets$dayOfYear)
##By minute
table(pathTweets$utcTime)
tweetFrequency <- table(pathTweets$easternTime)
write.table(tweetFrequency,file="tweetFrequency.csv",row.names=FALSE,sep=",")

library(chron)
summary(pathTweets$dayOfYear)
plot(hours(x=pathTweets$utcTime) + (pathTweets$dayOfYear - 298) * 24 )



##table(pathTweets$utcTime,Rdate$wday) See page 103

##Histogram of distances by frequency
qplot(km, data=pathTweets, geom="histogram", binwidth=100, xlab="Distance from Hurricane (km)", ylab="Number of Tweets",fill=I('steelblue'))

########################################################################################
##NOTES:
##Generating Frequency tables:
##http://www.statmethods.net/stats/frequencies.html
##
########################################################################################
##STEP FOUR: Distance to Cities
########################################################################################

##Read CSV
cityDistances <- read.csv("cityDistances.csv",header=T)
##Or
hurricanePath <- read.csv("hurricanePath.csv",header=T)
library(geosphere)

##Km to Miami
miami = NULL
for (i in 1:nrow(hurricanePath)) {
  miami[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(-80.2241,25.7877), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Km to Washington D.C.
washington = NULL
for (i in 1:nrow(hurricanePath)) {
  washington[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(-77.0367,38.8951), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Km to NYC
newYork = NULL
for (i in 1:nrow(hurricanePath)) {
  newYork[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(-75.1890,42.3482), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Km to Chicago
chicago = NULL
for (i in 1:nrow(hurricanePath)) {
  chicago[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(-87.6278,41.8819), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Km to Los Angeles
losAngeles = NULL
for (i in 1:nrow(hurricanePath)) {
  losAngeles[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(-118.2500,34.0500), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Km to London
london = NULL
for (i in 1:nrow(hurricanePath)) {
  london[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(-0.1275,51.5072), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Km to Moscow
moscow = NULL
for (i in 1:nrow(hurricanePath)) {
  moscow[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(37.6167,55.7500), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Km to Beijing
beijing = NULL
for (i in 1:nrow(hurricanePath)) {
  beijing[i] =
    (distVincentyEllipsoid(c(hurricanePath[i,2],hurricanePath[i,1]),c(116.3917,39.9139), a=6378137, b=6356752.3142, f=1/298.257223563))*0.001
}
##Use cbind to merge city distance calculations with hurricane path data
cityDistances <- cbind(hurricanePath,miami,washington,newYork,chicago,losAngeles,london,moscow,beijing)

##Save dataframe as CSV called pathTweets
write.csv(file="cityDistances.csv",x=cityDistances, row.names=FALSE)

########################################################################################
##STEP FOUR: Plot Tweets & Distance to Cities
########################################################################################

library(ggplot2)
library(directlabels)

##Plot Tweets by Distance
timeForPlotA <-as.POSIXct(pathTweets$easternTime)
ggplot() + xlab("East Coast Date/Time") + theme(axis.title.x=element_text(colour="grey50")) + ylab("Distance from Hurricane Eye (km)") + theme(axis.title.y=element_text(colour="grey50")) +
  theme(panel.grid.major=element_line(colour="white",size=0.25),panel.grid.minor=element_line(colour="grey90",size=0.25), panel.background=element_rect(fill="grey90")) +
  geom_point(data=pathTweets,aes(x=timeForPlotA,y=km),shape=".",size=0.01,colour="steelblue",alpha=1/3) + ylim(0,8000) + xlim(as.POSIXct("2012-10-24 20:00:00"), as.POSIXct("2012-10-31 12:00:00")) +
  geom_line(data=cityDistances,aes(x=timeForPlotB,y=miami,color="blue")) +
  geom_line(data=cityDistances,aes(x=timeForPlotB,y=washington,color="blue"))

##Plot Tweets & Cities by Distance
library(reshape2)
library(lattice)
timeForPlotB <-as.POSIXct(cityDistances$easternTime)
cityDistances$hurricanePath.lat <- NULL
cityDistances$hurricanePath.lon <- NULL
cityMelt <- melt(cityDistances)
timeForPlotA <-as.POSIXct(pathTweets$easternTime)
timeForPlotB <-as.POSIXct(cityMelt$easternTime)
p <- ggplot() + xlab("East Coast Date/Time") + theme(axis.title.x=element_text(colour="grey50")) + ylab("Distance from Hurricane Eye (km)") + theme(axis.title.y=element_text(colour="grey50")) +
  theme(panel.grid.major=element_line(colour="white",size=0.25),panel.grid.minor=element_line(colour="grey90",size=0.25), panel.background=element_rect(fill="grey90")) +
  geom_point(data=pathTweets,aes(x=timeForPlotA,y=km),shape=".",size=0.01,colour="steelblue",alpha=1/3) + ylim(0,14000) + xlim(as.POSIXct("2012-10-24 20:00:00"), as.POSIXct("2012-10-31 12:00:00")) +
  geom_line(data=cityMelt,aes(x=timeForPlotB,y=value,group=variable,color=variable))
p

##Add direct labels to cities (can't currently do with geom_points included)
p <- ggplot(data=cityMelt,aes(x=timeForPlotB,y=value,group=variable,color=variable)) + xlab("East Coast Date/Time") + theme(axis.title.x=element_text(colour="grey50")) + ylab("Distance from Hurricane Eye (km)") + theme(axis.title.y=element_text(colour="grey50")) +
  theme(panel.grid.major=element_line(colour="white",size=0.25),panel.grid.minor=element_line(colour="grey90",size=0.25), panel.background=element_rect(fill="grey90")) +
  xlim(as.POSIXct("2012-10-24 20:00:00"), as.POSIXct("2012-10-31 12:00:00")) +
  geom_line() 
print(direct.label(p,"last.points"))

##ARROWS! http://docs.ggplot2.org/0.9.3/geom_segment.html

##Intermediate coordinates
##intermediate1 <- gcIntermediate(c(noaaCoordinates[1,4],noaaCoordinates[1,3]),c(noaaCoordinates[2,4],noaaCoordinates[2,3]),n=5, addStartEnd=FALSE)
##write.csv(intermediate1,"path.txt")

##########################################################################################
##Mapping
##########################################################################################

pathTweets <- read.csv("pathTweets.csv",header=T)
require(rgdal)

##Read in shapefiles stored in the working directory
states <- readOGR(".", "states")
summary(states)

states <- spTransform(states, CRS("+init=epsg:4326"))
states <- states[-which(grepl("Alask|Haw", as.character(states$STATE_NAME))), ]
plot(states)
points(pathTweets$tweet.lon, pathTweets$tweet.lat, col = "blue",cex=.6)

##Convert the tweets into a spatial (S4) class
pathTweets <- SpatialPointsDataFrame(coords = matrix(c(pathTweets$tweet.lon, pathTweets$tweet.lat), 
           ncol = 2), data = pathTweets, proj4string = CRS("+init=epsg:4326"))

statesAg1 <- aggregate(mapTweets["X"], states, mean)
statesAg2 <- aggregate(mapTweets["actor.friendsCount"], by = states, mean)
statesAg1$friends <- statesAg2$actor.friendsCount
statesAg1$id <- as.character(states$STATE_NAME)

library(maptools)
library(gpclib)
gpclibPermit()
sf <- fortify(statesAg1, region = "id")

head(sf)
statesAg1 <- aggregate(mapTweets,states, length)
##This shows me that I've aggregated times, and calculated the mean for time, coordinates, etc.
head(statesAg1@data)

sf <- inner_join(sf, statesAg1@data, by = "id")
head(sf)

ggplot(sf, aes(long, lat, fill = friends, group = group)) + geom_polygon() + scale_fill_gradient(low = "yellow", 
                                                                                           high = "red") + coord_map()
statesAg2 <- aggregate(mapTweets["actor.friendsCount"], by = states, mean)
statesAg1$friends <- statesAg2$actor.friendsCount
statesAg1$id <- as.character(states$STATE_NAME)

##Visualisation

library(ggplot2)
library(dplyr)
library(maptools)

sf <- fortify(statesAg1, region = "id")












##Distance Histogram. Can also scale using '+ scale_y_sqrt()'.
p <- ggplot(pathTweets, aes(x=km, )) + scale_y_sqrt() +
  geom_histogram(binwidth=100, alpha=1, position="identity",fill="#330066") + theme_bw() +
  theme(plot.margin=unit(c(0,0,0,-0.5),"line"), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_rect(fill="white")) +
  facet_grid(dayOfYear ~ .) + xlim(0,10000) +
  labs(title='Distance Between Hurricane Position and Tweet Origins')
p + scale_x_continuous(expand = c(0, 0)) + scale_y_sqrt(expand = c(0, 0))
##flowingdata.com/2014/02/27/how-to-read-histograms-and-use-them-in-r/
##docs.ggplot2.org/0.9.3.1/geom_histogram.html
##www.cookbook-r.com/Graphs/Facets_(ggplot2)/
