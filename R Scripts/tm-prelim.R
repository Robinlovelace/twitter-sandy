# preliminary use of text mining applied to hurricane sandy
vc <- VCorpus(VectorSource(tweets$body))
vc <- tm_map(vc, FUN = content_transformer(FUN = tolower))
tdm <- TermDocumentMatrix(vc)
findAssocs(tdm, "sandy", 0.1)

help(topic = "lower", package = "tm")
