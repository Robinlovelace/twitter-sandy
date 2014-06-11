# script to convert strange mathematica .csv files into data frames

library(stringr) # load new string processing library
x <- readLines("data/testExport.txt")

  str_split(x[1], "\"}")[[1]][1]
  code <- str_split(x[1], "\"}")[[1]][1]
  code <- str_split(code, "for ")[[1]][2]
  # now extract data

d <- gsub("\\}, \\{\\{", "\n", x[1]) # split into separate rows
con <- textConnection(d)
d <- read.csv(con, skip = 1, header=F)  
close(con)
head(d)

s <- grep("for", d$V3) # the shifts (s) where the variable changes
d$V7 <- gsub("\\}\\}", "", d$V7) # remove confusing symbols

s.id <- (1: (s[1] - 1)) # ids of windspeed
speed <- as.numeric(as.character(d$V7[s.id] ))

g.id <- (s[1] + 1): (s[2] - 1) # ids of gusts
gusts <- as.numeric(as.character(d$V7[g.id] ))

t.id <- (s[2] + 1): (nrow(d) - 1)
temps <- as.numeric(as.character(d$V7[t.id] ))

e <- d[t.id,]
f <- d[g.id,]
head(e)
head(f)

# construct final data frame
df <- data.frame(yr = e$V1, mo = e$V2, day = e$V3, hr = e$V4, min = e$V5, speed, gusts, temps, code = code)
head(df)

# we've tested it on the first station, run for all
for(i in 2:length(x)){
  str_split(x[i], "\"}")[[1]][1]
  code <- str_split(x[i], "\"}")[[1]][1]
  code <- str_split(code, "for ")[[1]][2]
  # now extract data
  
  
  d <- gsub("\\}, \\{\\{", "\n", x[i]) # split into separate rows
  con <- textConnection(d)
  d <- read.csv(con, skip = 1, header=F)  
  close(con)
  head(d)
  
  s <- grep("for", d$V3) # the shifts (s) where the variable changes
  d$V7 <- gsub("\\}\\}", "", d$V7) # remove confusing symbols
  
  s.id <- (1: (s[1] - 1)) # ids of windspeed
  speed <- as.numeric(as.character(d$V7[s.id] ))
  
  g.id <- (s[1] + 1): (s[2] - 1) # ids of gusts
  gusts <- as.numeric(as.character(d$V7[g.id] ))
  
  t.id <- (s[2] + 1): (nrow(d) - 1)
  temps <- as.numeric(as.character(d$V7[t.id] ))
  
  e <- d[t.id,]
  dfNew <- data.frame(yr = e$V1, mo = e$V2, day = e$V3, hr = e$V4, min = e$V5, speed, gusts, temps, code = code)
  df <- rbind(df, dfNew)
}

summary(df) # it's got all the stations in there
  
