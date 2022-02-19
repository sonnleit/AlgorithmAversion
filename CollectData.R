

#----------------


#generic dataframe to store all tweets
alg <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(alg) <- c("text", "id", "created_at")



#----------------


for(x in 2010:2021){
  for (m in 1:12) {
    tmp <- get_all_tweets(
      "algorithm",
      start_tweets =paste(x,"-",m,"-01T00:00:01Z", sep = ""),
      end_tweets = paste(x,"-",m,"-",sample(2:28, 1),"T00:00:00Z", sep = ""),
      file = "algtweets",
      data_path = "data21/",
      lang="en",
      bearer_token = get_bearer(),
      n = 1000,
      bind_tweets = TRUE)
    alg <- rbind(alg, tmp[c("text", "id", "created_at")])
  }
}





#VADER sentiment analysis



algVADER <- vader_df(alg.data$text)
alg.data$VADER <- algVADER$compound

alg.data$VADER %>% length()


alg.data$VADERclass <- "Neutral"
alg.data$VADERclass[alg.data$VADER < -0.3 & alg.data$VADER > -0.7] <- "Negative"
alg.data$VADERclass[alg.data$VADER > 0.3] <- "Positive"
alg.data$VADERclass[alg.data$VADER <= - 0.7] <- "Aversive"


pattern.b <- "business|money|entrepreneur|CEO|innovation|trading|trade"
pattern.s <- "twitter|youtube|reddit|facebook|instagram|content|google"
pattern.t <- "decision|support|software|program|coding|scrum|network|engineer"
pattern.immu <- "immutable|persistent|stable|unchangeable|durable|continuity|unflexible"
pattern.infl <- "influence|effect|impact|control|changeability|changeable|controllable"
pattern.appl <- "apply|application|improvement|productive|pitch|set off"
pattern.averse <- "aversion|aversive|hate|disgusting|disgust|disfavor|dislike"


#initializing empty dataframe, that contains text for further analyzing
textdf <- data.frame(text=character(), stringsAsFactors = F)



for (i in seq(1,nrow(alg.data))){
  textdf <- rbind(textdf, tibble(text = alg.data$text[i]))
  
  clean.text <- textdf[i,1] %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) 
  
  alg.data[i, "Aversion"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.averse, word)))
  
  alg.data[i, "Business"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.b, word)))
  
  alg.data[i, "Social Media"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.s, word)))
  
  alg.data[i, "Technology"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.t, word)))
  
  alg.data[i, "Immutability"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.immu, word)))
  
  alg.data[i, "Influence"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.infl, word)))
  
  alg.data[i, "Application"] <- clean.text %>%
    summarize(count = sum(grepl(pattern.appl, word)))
}




#Create Year column to make filter easier

for(i in 2010:2021){
  alg.data$Year[alg.data$created_at >= paste(i, "01-01", sep="-") & alg.data$created_at <= paste(i+1, "01-01", sep="-")] <- as.character(i)
  
}


#Create index
alg.data <- rowid_to_column(alg.data, "index")




#Research question 1); descriptive

plot(summ.year$`alg.data$Year`, summ.year$Sent, xlab = 'Analyzed year', ylab = 'Tweets')
lines(unique(summ.year$`alg.data$Year`), summ.year$Sent[summ.year$`alg.data$VADERclass`=='Positive'], col = 3)
lines(unique(summ.year$`alg.data$Year`), summ.year$Sent[summ.year$`alg.data$VADERclass`=='Neutral'], col = 1)
lines(unique(summ.year$`alg.data$Year`), summ.year$Sent[summ.year$`alg.data$VADERclass`=='Negative'], col = 2)
lines(unique(summ.year$`alg.data$Year`), summ.year$Sent[summ.year$`alg.data$VADERclass`=='Aversive'], col = 4)

alg.data %>% group_by(alg.data$topic, alg.data$Year, alg.data$VADERclass) %>% summarise(Sent = n()) -> summ.data
alg.data %>% group_by(alg.data$Year, alg.data$VADERclass) %>% summarise(Sent = n()) -> summ.year
alg.data %>% group_by(alg.data$topic, alg.data$VADERclass) %>% summarise(Sent = n()) -> summ.topic


#Research question 2);




#-------------------

#-------------completed----------
alg.data %>% group_by(topic, Year, VADERclass) %>% summarise(Sent = n()) -> summ.data
alg.data %>% group_by(Year, VADERclass) %>% summarise(Sent = n()) -> summ.year

listofdfs <- list()

#data group for cummulated dataframe

for (i in c(7:12)){
  alg.data %>% 
    filter(alg.data[,i]>=1)%>% 
    mutate(Topic = colnames(alg.data)[i])%>% 
    group_by(Year, VADERclass, Topic) %>% 
    summarise(Sent = n()) ->listofdfs[[i]]
}
summ.cumm <- bind_rows(listofdfs)



#data group for topic oriented dataframe

listofdfs <- list()

for (i in c(7:12)){
  alg.data %>% 
    filter(alg.data[,i]>=1)%>% 
    mutate(Topic = colnames(alg.data)[i])%>% 
    group_by( VADERclass, Topic) %>% 
    summarise(Sent = n()) ->listofdfs[[i]]
}
summ.topic <- bind_rows(listofdfs)
summ.topic

#adds percentage to cummulated DF
summ.cumm <- 
  summ.cumm %>% 
  group_by(Topic, Year) %>% 
  mutate(All = sum(Sent),percent=(100*Sent/All))


#adds percentage to yearwise DF
summ.year<- 
  summ.year %>% 
  group_by(Year) %>% 
  mutate(All = sum(Sent),percent=(100*Sent/All))

#adds percentage to topicwise DF
summ.topic<- 
  summ.topic %>% 
  group_by(Topic) %>% 
  mutate(All = sum(Sent),percent=(100*Sent/All))

summ.cumm
summ.topic
summ.year

#-------------------Plotting------------------

#---------------------Year--------------------

plot(summ.year$Year, summ.year$percent, main ="Yearwise",xlab = 'Analyzed year', ylab = 'Percent')
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Positive'], col = 3)
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Neutral'], col = 1)
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Negative'], col = 2)
lines(unique(summ.year$Year), summ.year$percent[summ.year$VADERclass=='Aversive'], col = "blue")

legend("topright", legend=c("positive", "neutral", "negative", "aversive"), col=c("black","green", "red", "blue"), lty=1:1, cex=0.8)


#-----------------Topic-----------------------


