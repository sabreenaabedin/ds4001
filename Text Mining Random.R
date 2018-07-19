
library(tm)
library(wordcloud)
library(SnowballC)

dox <- Corpus(DirSource("C:/Users/Sabreena/Dropbox/DS/doc"),readerControl = list(language="eng"))

## Let's take a look at this corpus:
inspect(dox)
writeLines(as.character(dox))

#clean
dox <- tm_map(dox,tolower) 
dox <- tm_map(dox, PlainTextDocument)
dox <- tm_map(dox,stripWhitespace) 
dox <- tm_map(dox,removeWords,stopwords('english')) 
dox <- tm_map(dox,removePunctuation)

#stem
dox <- tm_map(dox,stemDocument)

## Let's see what our document looks like now:
writeLines(as.character(dox))

#wordcloud
wordcloud(dox,scale=c(5,0.5),max.words=100, random.order=FALSE,rot.per=.35,use.r.layout=FALSE,colors=brewer.pal(8,"Dark2"))

#analyze

dtm <- DocumentTermMatrix(dox)

## frequency
findFreqTerms(dtm,50) 
inspect(dtm)

findAssocs(dtm, "people", .8) 
