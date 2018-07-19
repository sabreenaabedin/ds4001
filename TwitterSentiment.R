
library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(plyr)
library(dplyr)
library(tm)
library(base64enc)
library(wordcloud)

key='ORUb35hObw5b1W64GB0yS48yK'
secret='84syK5DsvjSo4hn0rV7cE7Ybq2ZUIzJcWjRliNABXmgzP7j9Gy'
setwd("C:/Users/student/Desktop/Data Science")
access_token='447260452-jVKUVhBLx9wmj9NhdqhYVOwioSF10iImjHvcmXWJ'
access_token_secret='B9kvetC3EFDF3iOQYPtADClTri02zIX73THgWx1SRh8BA'

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/student/Desktop/Data Science/cacert.pem",
              method="curl")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret, access_token, access_token_secret)
save(authenticate, file="twitter authentication.Rdata")
# harvest some tweets
some_tweets = searchTwitter("speeddating", n=3000, lang="en")

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL


col=brewer.pal(6,"Dark2")
wordcloud(some_txt, min.freq=20, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

## Here we perform some sentiment analysis:

## First we inidicate lists of positive and negative words. These are located in your 
##"Day 14" Resources folder

positives= readLines("C:/Users/student/Desktop/Data Science/Data/positive_words.txt")
negatives= readLines("C:/Users/student/Desktop/Data Science/Data/negative_words.txt")

## Below is a function that scores sentiment on a scale of -5 to 5 (-5 being the most negative
## and 5 being the most positive).  A score is determined for each tweet based on its correlation
## with the positive words and the negative words.  This is original code written by a veteran R
##user that functions as part of an old package called "sentiment" that is no longer available.

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

## Now apply the function to our actual data.  

Score <- score.sentiment(some_txt,positives,negatives,.progress='none')

## Score has two fields: score and text.  We 
## are interested in score at this point, but we can look at a few of the tweets' text and
## the associated score first.

head(Score)

## Let’s plot a histogram of the sentiment score:
  
hist(Score$score,xlab="Sentiment Score ",main="Sentiment of sample tweets that have Trump in them ",
       border="black",col="skyblue")

## We can calculate overall sentiment by adding together all of the scores:

sum(Score$score)
 
## Finally, you can convert to a document term matrix and look for frequencies and associations:

text.corpus <- Corpus(VectorSource(some_txt))
dtm <- DocumentTermMatrix(text.corpus)



