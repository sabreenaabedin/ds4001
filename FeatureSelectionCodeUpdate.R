## In this code, we study the art of "feature selection," picking the best subgroup of features to use when building a model
## with the given data.

## We begin by removing correlations >75%.  Why are correlations unwanted? 

set.seed(7)
# load the library
installed.packages(mlbench)
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated) #gives index number of column which is highly correlated 


### Sort data according to order of importance
# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
PimaIndiansDiabetes$diabetes <- as.factor(PimaIndiansDiabetes$diabetes)
set.seed(12345)
PI_rand <- PimaIndiansDiabetes[order(runif(768)), ] #768 is number of instances in this set

# split the data frames
PI_train <- PI_rand[1:600, ]
PI_test  <- PI_rand[601:768, ]

## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)
PI_model <- C5.0(PI_train[-9], PI_train$diabetes)
# train the model
library(e1071)
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale")#, trControl=control)
# estimate variable importance
library(pROC)
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

## Here we look at all combinations of subsets of our original features to see which variables make the 
## strongest models

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
library(randomForest)
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
