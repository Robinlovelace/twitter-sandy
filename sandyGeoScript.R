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

##Bearing of tweet from eye relative to north
bearingToTweet = NULL
for (i in 1:129936) {
  bearingToTweet[i] =
    bearing(c(pathTweets[i,23],pathTweets[i,22]),c(pathTweets[i,3],pathTweets[i,2]))
}

##Neaten things up
##Remove superfluous columns from 'pathTweets'
pathTweets$utcTime.y <- NULL
pathTweets$utcTime.x <- NULL
##Rename utc2 to utcTime
library(plyr)
pathTweets <- rename(pathTweets, c("utc2"="utcTime"))
##Save dataframe as CSV called pathTweets
write.csv(file="pathTweets.csv",x=pathTweets, row.names=FALSE)

##Calculate bearing of tweets relative to bearing of hurricane
pathTweets$relativeBearingX = pathTweets$bearing - pathTweets$hurricaneBearing

relativeBearing = NULL
for (i in 1:129931){
  relativeBearing[i] =
    if(pathTweets[i,26] < 0) {pathTweets[i,26] + 360}
  else{pathTweets[i,26]}
}
pathTweets$relativeBearing <- NA
pathTweets$relativeBearing[1:129931] <- relativeBearing

##Remove superfluous columns from 'pathTweets'
pathTweets$relativeBearingX <- NULL

##Save dataframe as CSV called pathTweets
write.csv(file="pathTweets.csv",x=pathTweets, row.names=FALSE)

########################################################################################
##NOTES, GUIDANCE & REFERENCES:
##
##http://cran.r-project.org/web/packages/geosphere/geosphere.pdf
########################################################################################

########################################################################################
##Plot distance & bearing relative to hurricane eye: Close Up
########################################################################################

# Define function to calculate coordinates given distance and bearing
get.coords <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0+ d * sin(a / 180 * pi))
}

# Set up plotting device
plot.new()
par(mar=c(0, 0, 0, 0), oma=rep(0, 4),bg="white",col="white")
plot.window(xlim=c(-2100, 2100), ylim=c(-2100, 2100), asp=1)

# Add points
points(with(pathTweets, get.coords(relativeBearing, km, 0, 0)), pch=19, cex=0.1, col='steelblue')
##To instead plot a subset use something like this:
##subset(pathTweets, dayOfYear=="299")

# Semicircles with radii = 100 through 2000
sapply(seq(200, 4000, 200), function(x) {
  lines(get.coords(seq(0, 360, length.out=100), x, 0, 0),col="grey30")
})

# Horizontal line
segments(-3000, 0, 3000, 0,,col="grey30")
# Verticle line
segments(0,-3000,0,3000,col="grey30")
# Diagonal lines
segments(-3000,-3000,3000,3000,lty=2,col="grey30")
segments(-3000,3000,3000,-3000,lty=2,,col="grey30")

# Plot white curves over grey curves and add text
sapply(seq(400, 2000, 400), function(x) {
  txt <- paste0(x, 'km')
#  w <- strwidth(txt, cex=0.9)/2
#  a <- atan(w/x)/pi*180
#  lines(get.coords(seq(-a, a, length=5), x, 0, 0), 
#        lwd=20, col="white")
  text(0, x, txt, cex=0.8,col="grey30",font=2)
})

########################################################################################
##Plot distance & bearing relative to hurricane eye: Entire Globe
########################################################################################

# Define function to calculate coordinates given distance and bearing
get.coords <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0+ d * sin(a / 180 * pi))
}

# Set up plotting device
plot.new()
par(mar=c(0, 0, 0, 0), oma=rep(0, 4),bg="white",col="white")
plot.window(xlim=c(-20000, 20000), ylim=c(-20000, 20000), asp=1)

# Add points
points(with(pathTweets, get.coords(relativeBearing, km, 0, 0)), pch=19, cex=0.1, col='steelblue')

# Semicircles with radii = 100 through 2000
sapply(seq(1000, 20000, 1000), function(x) {
  lines(get.coords(seq(0, 360, length.out=100), x, 0, 0),col="grey30")
})

# Horizontal line
segments(-20000, 0, 20000, 0,,col="grey30")
# Verticle line
segments(0,-20000,0,20000,col="grey30")
# Diagonal lines
segments(-14000,-14000,14000,14000,lty=2,col="grey30")
segments(-14000,14000,14000,-14000,lty=2,,col="grey30")

# Plot white curves over grey curves and add text
sapply(seq(5000, 20000, 5000), function(x) {
  txt <- paste0(x, 'km')
  #  w <- strwidth(txt, cex=0.9)/2
  #  a <- atan(w/x)/pi*180
  #  lines(get.coords(seq(-a, a, length=5), x, 0, 0), 
  #        lwd=20, col="white")
  text(0, x, txt, cex=0.8,col="grey30",font=2)
})

########################################################################################
##NOTES, GUIDANCE & REFERENCES:
##
##http://stackoverflow.com/questions/22977453/plot-distance-and-bearing-in-r
########################################################################################

########################################################################################
##Plot Tweet Frequency Histogram
########################################################################################

##Read CSV
pathTweets <- read.csv("pathTweets.csv",header=T)

##Load Package
library(ggplot2)
library(grid) ##Required for the 'unit' function when adjusting margins
library(lubridate)

##Tweets by Hour as a histogram
y<-as.POSIXct(pathTweets$easternTime)
p<-qplot(y,xlim=c(as.POSIXct('2012/10/24 20:00:00'),as.POSIXct('2012/11/05 19:00:00')),binwidth=60*60,fill=I('steelblue'),xlab="Day (East Coast)",ylab="Tweets Per Hour")
print(p)

########################################################################################
##Create Frequency Table
########################################################################################

##By day
table(pathTweets$dayOfYear)
##By minute
table(pathTweets$utcTime)
tweetFrequency <- table(pathTweets$easternTime)
write.table(tweetFrequency,file="tweetFrequency.csv",row.names=FALSE,sep=",")

########################################################################################
##Plot Distance by Frequency Histogram
########################################################################################

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
