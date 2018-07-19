data("iris")

install.packages("cvTools")
library(cvTools) #run the above line if you don't have this library

k <- 10 #the number of folds
dataset <- iris
set.seed(1234)
dataset_rand <- dataset[order(runif(150)),]

## Divide data into k folds:

folds <- cvFolds(NROW(dataset), K=k)

dataset$holdoutpred <- rep(0,nrow(iris)) #explain this part

library(C50)
for(i in 1:k){
  train_data <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation_data <- dataset[folds$subsets[folds$which == i], ] #Set the validation set
  
  newmod <- C5.0(Species ~ .,data=train_data) #Get your new linear model (just fit on the train data)
  newpred <- predict(newmod,newdata=validation_data[,-5]) #Get the predicitons for the validation set (from the model just fit on the train data)
  
  dataset[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use.
}
dataset$holdoutpred #what does this tell us

table(dataset_rand$holdoutpred, dataset_rand$Species)

