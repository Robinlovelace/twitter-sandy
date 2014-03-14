library(geosphere)
bearing(c(41.93555928,-87.64283545),c(18.9,-76.4))
finalBearing(c(41.93555928,-87.64283545),c(18.9,-76.4))
distMeeus(c(-76.4,18.9),c(-87.64283545,41.93555928), a=6378137, f=1/298.257223563)
distVincentyEllipsoid(c(-76.4,18.9),c(-87.64283545,41.93555928), a=6378137, b=6356752.3142, f=1/298.257223563)
gcIntermediate(c(-87.64283545,41.93555928),c(-76.4,18.9), n=20, addStartEnd=TRUE)
midPoint(c(-87.64283545,41.93555928),c(-76.4,18.9))
randomCoordinates(n)
regularCoordinates(N)


destPoint(c(-76.4,18.9), 332.4, 1383467, r = 6378137)
distVincentyEllipsoid(c(-86.95590,40.86558),c(-86.29086,39.79165), a=6378137, b=6356752.3142, f=1/298.257223563)
distVincentyEllipsoid(c(-79.59048,26.67659),c(-79.11105,25.56976), a=6378137, b=6356752.3142, f=1/298.257223563)
gcIntermediate(c(-87.64283545,41.93555928),c(-76.4,18.9), n=20, addStartEnd=TRUE)


library(argosfilter)
bearing(18.9, 41.93555928, -76.4, -87.64283545)
distance(18.9, 41.93555928, -76.4, -87.64283545)

###################################################################################################################
##Charting Hurricane Path
###################################################################################################################

##Set working directory and read in NOAA Coordinates
setwd("C:\\Users\\ee12nr\\Desktop\\")
noaaCoordinates <- read.csv("noaaCoordinates.csv",header=T)

##Create a dataframe of intermediate coordinates for the first row## ##UPDATE FIRST NUMBER IN EACH [] AND FILE NAME##
intermediates <- gcIntermediate(c(noaaCoordinates[1,4],noaaCoordinates[1,3]),c(noaaCoordinates[2,4],noaaCoordinates[2,3]), n=noaaCoordinates[2,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_1.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[2,4],noaaCoordinates[2,3]),c(noaaCoordinates[3,4],noaaCoordinates[3,3]), n=noaaCoordinates[3,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_2.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[3,4],noaaCoordinates[3,3]),c(noaaCoordinates[4,4],noaaCoordinates[4,3]), n=noaaCoordinates[4,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_3.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[4,4],noaaCoordinates[4,3]),c(noaaCoordinates[5,4],noaaCoordinates[5,3]), n=noaaCoordinates[5,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_4.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[5,4],noaaCoordinates[5,3]),c(noaaCoordinates[6,4],noaaCoordinates[6,3]), n=noaaCoordinates[6,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_5.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[6,4],noaaCoordinates[6,3]),c(noaaCoordinates[7,4],noaaCoordinates[7,3]), n=noaaCoordinates[7,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_6.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[7,4],noaaCoordinates[7,3]),c(noaaCoordinates[8,4],noaaCoordinates[8,3]), n=noaaCoordinates[8,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_7.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[8,4],noaaCoordinates[8,3]),c(noaaCoordinates[9,4],noaaCoordinates[9,3]), n=noaaCoordinates[9,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_8.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[9,4],noaaCoordinates[9,3]),c(noaaCoordinates[10,4],noaaCoordinates[10,3]), n=noaaCoordinates[10,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_9.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[10,4],noaaCoordinates[10,3]),c(noaaCoordinates[11,4],noaaCoordinates[11,3]), n=noaaCoordinates[11,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_10.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[11,4],noaaCoordinates[11,3]),c(noaaCoordinates[12,4],noaaCoordinates[12,3]), n=noaaCoordinates[12,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_11.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[12,4],noaaCoordinates[12,3]),c(noaaCoordinates[13,4],noaaCoordinates[13,3]), n=noaaCoordinates[13,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_12.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[13,4],noaaCoordinates[13,3]),c(noaaCoordinates[14,4],noaaCoordinates[14,3]), n=noaaCoordinates[14,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_13.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[14,4],noaaCoordinates[14,3]),c(noaaCoordinates[15,4],noaaCoordinates[15,3]), n=noaaCoordinates[15,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_14.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[15,4],noaaCoordinates[15,3]),c(noaaCoordinates[16,4],noaaCoordinates[16,3]), n=noaaCoordinates[16,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_15.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[16,4],noaaCoordinates[16,3]),c(noaaCoordinates[17,4],noaaCoordinates[17,3]), n=noaaCoordinates[17,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_16.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[17,4],noaaCoordinates[17,3]),c(noaaCoordinates[18,4],noaaCoordinates[18,3]), n=noaaCoordinates[18,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_17.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[18,4],noaaCoordinates[18,3]),c(noaaCoordinates[19,4],noaaCoordinates[19,3]), n=noaaCoordinates[19,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_18.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[19,4],noaaCoordinates[19,3]),c(noaaCoordinates[20,4],noaaCoordinates[20,3]), n=noaaCoordinates[20,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_19.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[20,4],noaaCoordinates[20,3]),c(noaaCoordinates[21,4],noaaCoordinates[21,3]), n=noaaCoordinates[21,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_20.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[21,4],noaaCoordinates[21,3]),c(noaaCoordinates[22,4],noaaCoordinates[22,3]), n=noaaCoordinates[22,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_21.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[22,4],noaaCoordinates[22,3]),c(noaaCoordinates[23,4],noaaCoordinates[23,3]), n=noaaCoordinates[23,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_22.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[23,4],noaaCoordinates[23,3]),c(noaaCoordinates[24,4],noaaCoordinates[24,3]), n=noaaCoordinates[24,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_23.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[24,4],noaaCoordinates[24,3]),c(noaaCoordinates[25,4],noaaCoordinates[25,3]), n=noaaCoordinates[25,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_24.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[25,4],noaaCoordinates[25,3]),c(noaaCoordinates[26,4],noaaCoordinates[26,3]), n=noaaCoordinates[26,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_25.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[26,4],noaaCoordinates[26,3]),c(noaaCoordinates[27,4],noaaCoordinates[27,3]), n=noaaCoordinates[27,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_26.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[27,4],noaaCoordinates[27,3]),c(noaaCoordinates[28,4],noaaCoordinates[28,3]), n=noaaCoordinates[28,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_27.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[28,4],noaaCoordinates[28,3]),c(noaaCoordinates[29,4],noaaCoordinates[29,3]), n=noaaCoordinates[29,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_28.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[29,4],noaaCoordinates[29,3]),c(noaaCoordinates[30,4],noaaCoordinates[30,3]), n=noaaCoordinates[30,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_29.txt")

intermediates <- gcIntermediate(c(noaaCoordinates[30,4],noaaCoordinates[30,3]),c(noaaCoordinates[31,4],noaaCoordinates[31,3]), n=noaaCoordinates[31,5], addStartEnd=FALSE)
write.csv(intermediates,"C:\\Users\\ee12nr\\Desktop\\inter_results_30.txt")

##Experimental loop

library(geosphere)
setwd("E:\\I_Hurricane_Data\\")
noaaCoordinates <- read.csv("noaaCoordinates.csv",header=T)
interCoordResults = rep(NA, 35)
for (i in 1:nrow(noaaCoordinates))
{
  interCoordResults[i] = gcIntermediate(c(noaaCoordinates[i,3],noaaCoordinates[i,4]),c(noaaCoordinates[i+1,3],noaaCoordinates[i+1,4]), n=noaaCoordinates[i+1,5], addStartEnd=FALSE)
}


library(geosphere)
setwd("E:\\I_Hurricane_Data\\")
noaaCoordinates <- read.csv("noaaCoordinates.csv",header=T)
interCoordResults = rep(NA, 35)
for (i in 1:nrow(noaaCoordinates))
{
  interCoordResults[i] <- gcIntermediate(c(noaaCoordinates[i,3],noaaCoordinates[i,4]),c(noaaCoordinates[i+1,3],noaaCoordinates[i+1,4]), n=noaaCoordinates[i+1,5], addStartEnd=FALSE)
  write.csv(interCoordResults[i],paste("E:\\I_Hurricane_Data\\Test\\",i,".csv",sep="")
}

###################################################################################################################
##Matching Hurricane Location to Tweets By Time
###################################################################################################################

##Create subsets matched by time

setwd("E:\\I_Hurricane_Data\\Hurricane_Location_Merge\\")
sandyPath <- read.csv("all-coordinates.csv",header=T)
firstMinPath <- subset(sandyPath, day==25 & hour==00 & min>=00 & min <10, select=c(lat,lon,day,hour,min))

setwd("E:\\I_Hurricane_Data\\")
sandyTweets <- read.csv("sandyGeoHrFormatted.csv",header=T)
firstMinTweets <- subset(sandyTweets, day==25 & hour==00 & min>=00 & min <10, select=c(lat,lon,day,hour,min))

########################
##Subset Loop Experiment
########################

#In this experiment, I'll create subsets of all 00:00:00-00:00:10 tweets from any day, then try to join the path coordinates from 25 to just the first day

setwd("E:\\I_Hurricane_Data\\Hurricane_Location_Merge\\")
sandyPath <- read.csv("all-coordinates.csv",header=T)
firstMinPath <- subset(sandyPath, hour==00 & min>=00 & min <10, select=c(lat,lon,day,hour,min))

setwd("E:\\I_Hurricane_Data\\")
sandyTweets <- read.csv("sandyGeoHrFormatted.csv",header=T)
firstMinTweets <- subset(sandyTweets, hour==00 & min>=00 & min <10, select=c(lat,lon,day,hour,min))

if(firstMinPath[["day"]]==firstMinTweets[["day"]]){1+1}


xx <- 8
xy <- 8
yx <- 5
yy <- 4

if(xx==xy & yx==yy){xx+xy} ##If both values match, perform the function in the {}

###################################################################################################################
##Calculating Distance & Bearings
###################################################################################################################

##Calculate distance between hurricane & each tweet

library(geosphere)
distanceResults = NULL
for (i in 1:nrow(firstMinTweets)) {
  distanceResults[i] =
    distVincentyEllipsoid(c(firstMinPath[1,2],firstMinPath[1,1]),c(firstMinTweets[i,2],firstMinTweets[i,1]), a=6378137, b=6356752.3142, f=1/298.257223563)
}

##Calculate initial bearing between hurricane & each tweet

initialBearingResults = NULL
for (i in 1:nrow(firstMinTweets)) {
  initialBearingResults[i] =
    bearing(c(firstMinPath[1,2],firstMinPath[1,1]),c(firstMinTweets[i,2],firstMinTweets[i,1]))
}

##Calculate final bearing between hurricane & each tweet

finalBearingResults = NULL
for (i in 1:nrow(firstMinTweets)) {
  finalBearingResults[i] =
    finalBearing(c(firstMinPath[1,2],firstMinPath[1,1]),c(firstMinTweets[i,2],firstMinTweets[i,1]))
}

##Pull results together into a single data frame to export

overallResults <- data.frame(firstMinTweets,distanceResults,initialBearingResults,finalBearingResults)













################

#library(xts)
#sandyTweets$postedTime <- strptime(sandyTweets$postedTime, format = "%Y-%m-%dT%H%M%S")