# Code to perform sentiment analysis on
# Twitter hashtags from Uganda. 
#__Author_ "ngamita@gmail.com"


if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


# @ngamita account 9pm
api_key = "hWxwDIUYbrJVf3LN2KvZpg"
api_secret = "Bj8TitB7ZFeqdFbkxmPaI5dXM9WYsFT9rPpDju7ODaA"
access_token = "73121672-bFHzS58KtuLidOtuSMRi99bsi1tS2V3qexqbRInDs"
access_token_secret = "HbxxNqItgqJ3MdD3ube2pTHoedR4dVSIyB9euR2xZKIDc"

# @kampalr at 10pm
api_key = "JE5vaMAgzPbaoCxwhGx0B9QbR"
api_secret = "oN9B0jnDo4EvtxZajRH9D7ShgwvTmz3oOuS8oL9txScRMOUV6R"
access_token = "3910492041-7v48HkvTcRQ3jlRM6aOgdhv1BXFuLFzBZSnwYm6"
access_token_secret = "vA5fPLV0bbu9akqHB3wYNM0L7TRIMctKXazA1RydqGzGO"

# @richardzulu
api_key = "jlJ9rOHbH96vJDvkR45G1aYAM"
api_secret = "XbDuJy0hmLuCkLL8eMGPRas9iskJLnLjFG1S4e9o3ah4YT1u6x"
access_token = "56692528-ExKDYjbbXJNCSu3iJ6pgV2xWwhAViMjYfzu5tYuBi"
access_token_secret = "qTOcJETFrbyvcoUOqZKXJhWu8XLhIjfkBlrSeJwep7BA4"




setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# harvest some tweets
#u_tweets = searchTwitter("UGDebate16", n=10000, lang="en")
# u_tweets <- read.csv('~/home/ngamita/UGDebate16v2/u_tweets_1.csv')


# Get the files names
files = list.files(pattern="*.csv")

# First apply read.csv, then rbind
u_tweets = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))


# Write to a file or dump. 
#u_tweets_df <- do.call("rbind", lapply(u_tweets, as.data.frame))
#write.csv(u_tweets_df,file="u_tweets_9pm_7.csv")

# get the text
u_txt = sapply(u_tweets, function(x) x$getText())

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

# Let’s do some plots of the obtained results
# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets about Uganda Decides\n(classification by emotion)") +
  theme(plot.title = element_text(size=12, face="bold"))


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  ggtitle("Sentiment Analysis of Tweets about Uganda Decides\n(classification by polarity)") +
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

# emo.docs <- iconv(emo.docs, 'UTF-8', 'ASCII')
# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tm_map(corpus, removePunctuation)
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

comparison.cloud(tdm, random.order=FALSE,colors = c("#00B2FF", "red","#FF0099","#6600CC"), max.words=500)


m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing = TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word = names(word_freqs), freq = word_freqs)
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))



# get the text
u_txt_raw = sapply(u_tweets, function(x) x$getText())

# characters per tweet
chars_per_tweet = sapply(u_txt_raw, nchar)
#barplot(summary(chars_per_tweet))


# split words
words_list = strsplit(u_txt_raw, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
        main="Distribution of words per #UgandaDecides tweet", cex.main=1)

# length of words per tweet
wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))

# barplot
barplot(table(round(wsize_per_tweet)), border=NA,
        xlab = "word length in number of characters",
        main="Distribution of words length per #UgandaDecides tweet", cex.main=1)


# how many unique words per tweet
uniq_words_per_tweet = sapply(words_list, function(x) length(unique(x)))
# barplot
barplot(table(uniq_words_per_tweet), border=NA,
        xlab = "unique words",
        main="Distribution of unique words per # UgandaDecides tweet", cex.main=1)

# how many hashtags per tweet
hash_per_tweet = sapply(words_list, function(x) length(grep("#", x)))
table(hash_per_tweet)
prop.table(table(hash_per_tweet))

# how many @mentions per tweet
ats_per_tweet = sapply(words_list, function(x) length(grep("@", x)))
table(ats_per_tweet)
prop.table(table(ats_per_tweet))


# data frame
icedf = data.frame(
  chars=chars_per_tweet,
  words = words_per_tweet,
  lengths = wsize_per_tweet,
  uniqs = uniq_words_per_tweet,
  hashs = hash_per_tweet,
  ats = ats_per_tweet,
)


# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)

# barplot
barplot(top20, border=NA, las=2, main="Top 20 most frequent #UgandaDecide tweet terms", cex.main=1)

# words -vs- chars
ggplot(icedf, aes(x=words, y=chars)) +
  geom_point(colour="gray20", alpha=0.2) +
  stat_smooth(method="lm") +
  labs(x="number of words per tweet", y="number of characters per tweet") +
  opts(title = "Tweets about 'icecream' \nNumber of words -vs- Number of characters",
       plot.title = theme_text(size=12))