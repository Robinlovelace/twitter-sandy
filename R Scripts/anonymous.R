# Script to generate anonymous, scrambled tweets
tweets.orig <- tweets

names(tweets) # what are the variables?

tweets$X <- NULL
tweets$actor.preferredUsername <- NULL
tweets$generator.displayName <- NULL
tweets$gnip.klout_score <- NULL
tweets$twitter_entities.hashtags.text <- NULL
tweets$inReplyTo.link <- NULL
tweets$link <- NULL
tweets$object.actor.displayName <- NULL
tweets$object.actor.preferredUsername <- NULL
tweets$object.twitter_entities.hashtags.text <- NULL
tweets$postedTime <- NULL
tweets$twitter_entities.user_mentions.screen_name <- NULL
tweets$actor.link <- NULL
tweets$sec <- NULL
tweets$verb <- NULL

tweets$body <- as.character(tweets$body)

library(tm)
help(package = "tm") # read help on tm packages
body2 <- removePunctuation(tweets$body) # strip out punctuation

vc <- VCorpus(VectorSource(body2))
inspect(vc[1:3])
dtm <- DocumentTermMatrix(vc)
inspect(dtm[1:10,400:405])

lfts <- findFreqTerms(dtm, lowfreq = 1, highfreq = 2) # low frequency terms
head(lfts)

for(i in 1:length(lfts)){
  body2 <- gsub(pattern = lfts[i], replacement = "uniword", body2)
}

vc <- VCorpus(VectorSource(body2))
inspect(vc[1:3])
dtm <- DocumentTermMatrix(vc)

lfts <- findFreqTerms(dtm, lowfreq = 1, highfreq = 2) # low frequency terms
head(lfts)

allwords <- findFreqTerms(dtm, lowfreq = 1, highfreq = 20)
allwords[sample(length(allwords), size = 10)]

tweets$body <- body2
body3 <- strtrim(body2, width = 50)

head(tweets[-6])
tweets$lat <- as.numeric(as.character(tweets$lat))
tweets$lon <- as.numeric(as.character(tweets$lon))

tweets <- tweets[!is.na(tweets$lat), ]
tweets <- tweets[!is.na(tweets$lon), ]
write.csv(tweets, file = "/tmp/andy-n-tweets.csv")

library(sp)

p <- SpatialPointsDataFrame(coords = matrix(c(tweets$lon, tweets$lat), ncol = 2), data = tweets, proj4string = CRS("+init=epsg:4326"))
plot(p)

library(rgdal)
writeOGR(p, "/tmp", "tweets-shp", driver = "ESRI Shapefile")
