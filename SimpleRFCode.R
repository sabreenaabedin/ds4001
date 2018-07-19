## Random Forest Code 

#################FIRST DATA SET (TO STUDY IMPORTANCE USING RF) ######
## Set seed and load data
library(randomForest)
set.seed(4543)
data(mtcars)
## Create Random Forest
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
                          importance=TRUE)
## Examine importance in terms of both %IncMSE and increase in node purity.
importance(mtcars.rf) #percent increase in MSE when an attribute is taken away
importance(mtcars.rf, type=2) #increase node purity 
varImpPlot(mtcars.rf)

##Split data into testing and
##### DIFFERENT DATA SET ############################################

data(iris)
set.seed(111)

##Split data into testing and training data using an 80%/20% split
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))
  #split into two sets, number of nows in iris (all rows), first set 80% second 20%
  #already adds randomization

##Build your random forest
iris.rf <- randomForest(Species ~ ., data=iris[ind == 1,],ntree = 500)
  #based on training data (set 1)

##Make predictions on iris Species based on the random forest you just built.
iris.pred <- predict(iris.rf, iris[ind == 2,])
  #based on testing data 

##How well did we do?
table(observed = iris[ind==2, "Species"], predicted = iris.pred)
