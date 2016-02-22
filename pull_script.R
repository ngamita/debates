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
u_tweets = searchTwitter("#Ugandadecides", n=10000, lang="en")


# Write to a file or dump. 
u_tweets_df <- do.call("rbind", lapply(u_tweets, as.data.frame))
write.csv(u_tweets_df,file="pre_election_14_27.csv")