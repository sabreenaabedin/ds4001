# load the data
library(mlbench)
library(caret)
library(C50)
library(e1071)
library(pROC)
library(ggplot2)

data(PimaIndiansDiabetes)
PID <- PimaIndiansDiabetes
View(PID)
#randomize and split the data
PID$diabetes <- as.factor(PID$diabetes)
set.seed(3)
PID_rand <- PID[order(runif(768)), ]      #768 is number of instances in this set
PID_train <- PID_rand[1:576, ]      # split testing and training data
PID_test  <- PID_rand[577:768, ]
prop.table(table(PID_train$diabetes)) #check distribution of data sets
prop.table(table(PID_test$diabetes))

#create and test the model
model <- C5.0(PID_train[-9], PID_train$diabetes)
pred <- predict(model, PID_test) #test model
library(gmodels)
CrossTable(PID_test$diabetes, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetes', 'predicted diabetes'))

accuracy <- (96 + 40)/192
accuracy

#do attribute selection, test again, show confusion matrix

#use a random forest algorithm to select key attributes
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
library(randomForest)
results <- rfe(PID[,1:8], PID[,9], sizes=c(1:8), rfeControl=control) #1-8 are attributes while 9 is the outcome
print(results)
predictors(results) #pedigree, triceps, and pressure are the least important

#remove least important attributes and recreate the model
View(PID)
subPima <- PID[, c(-3, -4, -7)]
subPima$diabetes <- as.factor(subPima$diabetes)
View(subPima)

set.seed(3)
subPima_rand <- PID[order(runif(768)), ] #768 is number of instances in this set
sub_train <- subPima_rand[1:576, ]
sub_test  <- subPima_rand[577:768, ]

new_model <- C5.0(sub_train[-9], sub_train$diabetes) #create model
pred2 <- predict(new_model, sub_test) #test model
CrossTable(sub_test$diabetes, pred2, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diabetes', 'predicted diabetes'))
accuracy2 <- (96 + 40)/192 #from the yes/yes and no/no sections of the cross tables

accuracy
accuracy2
diff <- accuracy - accuracy2 #compare accuracies
diff

#the accuracy did not change at all between the two models, showing that the least important 
#three attributes were all providing the model with redundant data
