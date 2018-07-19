## First, we install the packages for text mining (tm) and wordcloud creation:

install.packages("tm")
install.packages("wordcloud")
library(tm)
library(wordcloud)

## Now we join the document(s) we have in our special reserved folder into a corpus.
lords <- Corpus(DirSource("C:/Users/Sabreena/Dropbox/DS/doc"),readerControl = list(language="eng"))

## Let's take a look at this corpus:
inspect(lords)
writeLines(as.character(lords))

## Now, to prepare the document for evaluation and for creation of a DocumentTermMatrix (dtm), a useful 
##format of the original text, we use the "tm_map" command.
lords <- tm_map(lords,tolower) ## this command makes all words in the document lower case
lords <- tm_map(lords, PlainTextDocument) ## here, we ensure that lords is a plain text document
lords <- tm_map(lords,stripWhitespace) ## here we are removing extra white space from the document

library(SnowballC)
lords <- tm_map(lords,stemDocument) ## this command stems words, e.g. "company" and "companies" become "compan".
## we will not use the above command, however, for creating our word cloud.
lords <- tm_map(lords,removeWords,stopwords('english'))  ## this command gets rid of common participles and
## words that are not of particular utility in evalulating a document (e.g. "a" and "an")
## Finally, we remove punctuation:
lords <- tm_map(lords,removePunctuation)
## Let's see what our document looks like now:
writeLines(as.character(lords))

## Now we make our wordcloud.
install.packages("SnowballC")
library(SnowballC)

wordcloud(lords,scale=c(5,0.5),max.words=100, random.order=FALSE,rot.per=.35,use.r.layout=FALSE,colors=brewer.pal(8,"Dark2"))

## Here we create the Document Term Matrix (dtm) for "lords".  In this format, we can begin to perform all sorts
## of analyses on our text.

dtm <- DocumentTermMatrix(lords)

## Let's look at the high frequency words in the document:
findFreqTerms(dtm,10) ## With this command, we are finding words that occur at least 10 times in the text.
inspect(dtm)

findAssocs(dtm, "nobel", 0.8) #only works if you have multiple documents in your file
