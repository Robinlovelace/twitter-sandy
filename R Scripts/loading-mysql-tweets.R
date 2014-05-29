# load twitter db
library(RMySQL)
con <- dbConnect(dbDriver("MySQL"), user = "root", password = "NDean19", dbname = "twitter")
## list the tables in the database
dbListTables(con)

tweets1 <- dbReadTable(con, "tweets_1")
head(tweets1)
save.image("tweets1.RData")
