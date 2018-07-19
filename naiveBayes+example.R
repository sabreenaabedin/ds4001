## Let's study votes from the House of Representatives from 1984.

## Open the data and look at it:
data(HouseVotes84, package ="mlbench")
head(HouseVotes84, 5)
library(e1071)
str(HouseVotes84)
table(HouseVotes84$Class)

# Factor the class variables
HouseVotes84$Class <- factor(HouseVotes84$Class)
str(HouseVotes84$Class)
table(HouseVotes84$Class)

## Now, look at the total number of yes/no votes cast for the first issue
plot(as.factor(HouseVotes84[,2]))
title(main="Total Vote cast for issue 1", xlab="vote", ylab="# reps")

## How did the Republicans vote on the first issue?
plot(as.factor(HouseVotes84[HouseVotes84$Class=='republican', 2]))
title(main='Republican votes cast for issue 1', xlab='vote', ylab='#reps')

## How did the Democrats vote on the same issue?
plot(as.factor(HouseVotes84[HouseVotes84$Class=='democrat', 2]))
title(main='Democrat votes cast for issue 1', xlab='vote', ylab='#reps')

na_by_col_class <- function (col, cls) {
  return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class==cls))
}
#let first 400 are training data and last 35 are test data
data_rand <- HouseVotes84[order(runif(435)), ]
trainingdata <- data_rand[1:350,]
testdata <- data_rand[350:435,]

table(HouseVotes84$Class)

table(trainingdata$Class)
table(testdata$Class)

library(e1071)

## Break down the training data for each issue into posterior probabilities:
modelposteriorprob <- naiveBayes(Class ~ ., data = trainingdata, type = "raw")
modelposteriorprob

## Create your naiveBayes model (remember that this is a query-based model):
nbmodel <- naiveBayes(Class ~ ., data = trainingdata)
nbmodel

#  Based on your model, now make predictions on test data
nbresult <- predict(nbmodel, testdata)
prop.table(table(nbresult))

library(caret)
library(lattice)
library(ggplot2)
library(gmodels)

nbcm<-confusionMatrix(nbresult, testdata$Class)
str(nbcm)
nbcm 

## Try to improve results by using Laplace smoothing:
modelposteriorprob2 <- naiveBayes(Class ~ ., data = trainingdata, type = "raw", laplace = 1)

modelposteriorprob2
nbmodel2 <- naiveBayes(Class ~ ., data = trainingdata)
nbmodel2
nbresult2 <- predict(nbmodel2, testdata)
nbresult2

prop.table(table(nbresult2))
nbcm2<-confusionMatrix(nbresult2, testdata$Class)

str(nbcm2)
nbcm2

tab1<-table(nbresult2,testdata$Class)
tab1
