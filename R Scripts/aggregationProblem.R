# make this save the state associated with each tweet into "mapTweets"
statesAg1 <- aggregate(mapTweets["X"], states, mean)
statesAg2 <- aggregate(mapTweets["actor.friendsCount"], by = states, mean)
statesAg1$friends <- statesAg2$actor.friendsCount
statesAg1$id <- as.character(states$STATE_NAME)

# clipping the tweets
