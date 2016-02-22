
# Re-categorizing tweets per candidate. 
# Create per candidate columns to avoid overwrites. 
library(stringr)
tweets$besigye <- FALSE
tweets[(str_detect(tweets$text, c('besigye', 'Besigye'))),19] <- TRUE

tweets$museveni <- FALSE
tweets[(str_detect(tweets$text, c('museveni', 'Museveni'))),20] <- TRUE

tweets$mbabazi <- FALSE
tweets[(str_detect(tweets$text, c('mbabazi', 'Mbabazi'))),21] <- TRUE

tweets$mabirizi <- FALSE
tweets[(str_detect(tweets$text, c('mabirizi', 'Mabirizi'))),22] <- TRUE

tweets$kyalya <- FALSE
tweets[(str_detect(tweets$text, c('kyalya', 'Kyalya'))),23] <- TRUE

tweets$bwanika <- FALSE
tweets[(str_detect(tweets$text, c('bwanika', 'Bwanika'))),24] <- TRUE

tweets$biraaro <- FALSE
tweets[(str_detect(tweets$text, c('biraaro', 'Biraaro'))),25] <- TRUE

tweets$baryamureeba <- FALSE
tweets[(str_detect(tweets$text, c('baryamureeba', 'Baryamureeba'))),26] <- TRUE


# Creat a dataframe of mentions of top candidates. 
candidates <- c('besigye', 'museveni', 'kyalya', 'bwanika', 'mabirizi', 'baryamureeba', 'biraaro', 'mbabazi')
mentions <- c(length(which(tweets$besigye)),length(which(tweets$museveni)),length(which(tweets$kyalya)),length(which(tweets$bwanika)),
                length(which(tweets$mabirizi)),
                length(which(tweets$baryamureeba)),
                length(which(tweets$biraaro)),
                length(which(tweets$mbabazi)))

candidatesdf <- data.frame(candidates, mentions)
candidatesdf<-arrange(candidatesdf,desc(mentions))

barplot(candidatesdf$mentions, names.arg = candidatesdf$candidates,beside = TRUE, col = 1:6, space = c(0, 2), las=2)


# Candidates sentiments. 
# Narrow down to candidates column on DF. 

tweets$candidate <- FALSE



for(i in 1:nrow(tweets)){
  if(tweets$besigye[i] || tweets$museveni[i] || tweets$biraaro[i] || tweets$mabirizi[i] || tweets$kyalya[i] ||
       tweets$bwanika[i] || tweets$mbabazi[i] || tweets$baryamureeba[i]){
    tweets$candidate[i] <- TRUE
  }
}

# Tweets with candidate mentions ~23k
tweets_c <- tweets[tweets$candidate == TRUE,]


library(tm)
library(stringr)
library(wordcloud)
library(SnowballC)

# u_tweets <- read.csv('u_tweets_2.csv', stringsAsFactors=FALSE)
u_txt <- tweets_c$text


# remove retweet entities
u_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", u_txt)

# remove at people
u_txt = gsub("@\\w+", "", u_txt)

# remove punctuation
u_txt = gsub("[[:punct:]]", "", u_txt)

# remove numbers
u_txt = gsub("[[:digit:]]", "", u_txt)

# remove html links
u_txt = gsub("http\\w+", "", u_txt)

# remove unnecessary spaces
u_txt = gsub("[ \t]{2,}", "", u_txt)
u_txt = gsub("^\\s+|\\s+$", "", u_txt)

# Define error handling function. 
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

# lower case using try.error with sapply
u_txt = sapply(u_txt, catch.error)

# remove NAs in some_txt
u_txt = u_txt[!is.na(u_txt)]
names(u_txt) = NULL

if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)


# Run Naive Bayes classifier on tweets. 
# Perform Sentiment Analysis
# classify emotion
class_emo = classify_emotion(u_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(u_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# Create data frame with the results and obtain some general statistics
# data frame with results
sent_df = data.frame(text=u_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


dev.off()
# Let’s do some plots of the obtained results
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets UGDebate16 \n(classification by emotion)") +
  theme(plot.title = element_text(size=12, face="bold"))

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets #UGDebate16 \n(classification by polarity)") +
  theme(plot.title = element_text(size=12, face="bold"))

# Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = u_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
#emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tm_map(corpus, removePunctuation)
tm_map(corpus, removeWords, c(stopwords("english"),"you","the","for"), lazy=TRUE) 
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

comparison.cloud(tdm, random.order=FALSE,colors = c("#00B2FF", "red","#FF0099","#6600CC"), max.words=500)

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
tweets_c$text <- iconv(tweets_c$text,"WINDOWS-1252","UTF-8")
mySentiment <- get_nrc_sentiment(tweets_c$text)
head(mySentiment)
tweets_c <- cbind(tweets_c, mySentiment)


# Total sentiment scores as per 8 emotions. 

sentimentTotals <- data.frame(colSums(tweets_c[,c(33:40)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")




library(SnowballC)

## Option 1: retrieve tweets from Twitter
library(twitteR)
library(tm)



# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets_c$text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy=TRUE)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation, lazy=TRUE) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers, lazy=TRUE)

# remove funny alnum shit.
#removelALNUM <- function(x) gsub("[^[:alnum:]///' ]", "", x)

# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL), lazy=TRUE)  #??
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via", "you")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("ugdebate", "uganda"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords, lazy=TRUE)
#

myCorpus <- tm_map(myCorpus, PlainTextDocument)
#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
#myCorpus <- tm_map(myCorpus, stemDocument, lazy=TRUE)



# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))
}

#myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)


#mbabaziCases <- tm_map(myCorpusCopy,grep, pattern = "\\<mbabazi")
#sum(unlist(miningCases))

## count frequency of "miners"
#minerCases <- tm_map(myCorpusCopy, grep, pattern = "\\<miners")
#sum(unlist(minerCases))


# # replace "miners" with "mining"
# myCorpus <- tm_map(myCorpus, gsub, pattern = "miners", replacement = "mining")

#tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

## Freqency words and Association
idx <- which(dimnames(tdm)$Terms == "ugdebate")
inspect(tdm[idx + (0:5), 101:110])

(freq.terms <- findFreqTerms(tdm, lowfreq=15))


term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=5)
df <- data.frame(term = names(term.freq), freq = term.freq)
df <- df[df$freq > 500,]

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

findAssocs(tdm, "mbabazi", 0.2)


# Word Associations. 
library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)

# Errors


library(wordcloud)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
