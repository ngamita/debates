# Who was driving th content. 

theString <- tweets$text
theString1 <- unlist(strsplit(theString, " "))
regex1 <- "(^|[^@\\w])@(\\w{1,15})\\b" # get strings with @
regex2 <- "[^[:alnum:]@_]"             # remove all punctuation except _ and @
users <- gsub(regex2, "", theString1[grep(regex1, theString1, perl = T)])
users

# Count @accounts that appear most.
counts=table(users)
barplot(counts)

# Let's do something hacky:
# Limit the data set to show only folk who tweeted twice or more in the sample
cc=subset(counts,counts>100)
cc_df <- as.data.frame(as.table(cc))
names(cc_df) <- c('screenname', 'counttweets')

barplot(cc_df$counttweets, las=2, names.arg=cc_df$screenname, axes=FALSE)
barplot(cc_df$counttweets, las=2, cex.names=0.8, names.arg=cc_df$screenname, axes=TRUE)

# Arrange in Descending. 
library(dplyr)
library(tidyr)
#supposing you want to arrange column 'c' in descending order and 'd' in ascending order. name of data frame is df
## first doing descending
df<-arrange(cc_df,desc(counttweets))
df <- df[1:30, ]

barplot(df$counttweets, las=2, names.arg=df$screenname, axes=FALSE)
barplot(df$counttweets, las=2, cex.names=0.8, names.arg=df$screenname, axes=TRUE)




# Top bots 
# Count screen name. Who the top people re-tweeting as bots at the UGDebates16 
t =tweets[tweets$retweetCount > 200,]
t$screenName
write.csv(t$screenName,file="bots_debate_2.csv")



# Sentiments Trust cloud


library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
tweets$text <- iconv(tweets$text,"WINDOWS-1252","UTF-8")
mySentiment <- get_nrc_sentiment(tweets$text)
head(mySentiment)
tweets <- cbind(tweets, mySentiment)
