library(cvTools)
library(randomForest)
library(ROCR)
library(e1071)
library(pROC)
library(caret)
library(C50)
library(DMwR)

##Load data and add appropriate labels
BCData <- read.csv("C:/Users/Sabreena/Dropbox/DS/WisconsinBC.txt", header=FALSE)
colnames(BCData) <- c('id','thickness','size','shape','adhesion','single','nuclei','chromatin','nucleoli','mitoses','class')
BCData$class<- factor(BCData$class, levels = c(2,4),labels = c("benign", "malignant"))
BCData[BCData == "?"] <- NA
summary(BCData)


k <- 10 #the number of folds
set.seed(12354)
BCData[1] <- NULL #remove identifier column

#### C5.0 decision tree model #########

##Perform cross validation, k=10 folds
BC_rand <- BCData[order(runif(699)), ]
folds <- cvFolds(NROW(BCData), K=k)
BC_rand$holdoutpred <- rep(0,nrow(BC_rand))
BC_rand$class <- as.factor(BC_rand$class)
for(i in 1:k){
  train_data <- BC_rand[folds$subsets[folds$which != i], ] #Set the training set
  validation_data <- BC_rand[folds$subsets[folds$which == i], ] #Set the validation set
  newmod <- C5.0(class ~ .,data=train_data) #Get your new linear model (just fit on the train data)
  newpred <- predict(newmod,newdata=validation_data[,-10]) #Get the predicitons for the validation set (from the model just fit on the train data)
  BC_rand[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use.
}

pred=prediction(BC_rand$holdoutpred,BC_rand$class)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE))) ##AUC = 0.93558
BC_rand$holdoutpred <- factor(BC_rand$holdoutpred, levels = c(1,2), labels = c("benign", "malignant"))
confusionMatrix(BC_rand$holdoutpred,BC_rand$class) ##accuracy = 0.9413

##### random forest ########

##Perform cross validation, k=10 folds
BC_rand <- BCData[order(runif(699)), ]
BC_rand <- BC_rand[complete.cases(BC_rand),] ##random forest can't handle NAs
folds <- cvFolds(NROW(BC_rand), K=k)
BC_rand$holdoutpred <- rep(0,nrow(BC_rand))

for(i in 1:k){
  train_data <- BC_rand[folds$subsets[folds$which != i], ] #Set the training set
  validation_data <- BC_rand[folds$subsets[folds$which == i], ] #Set the validation set
  
  newmod <- randomForest(class ~ ., train_data)
  newpred <- predict(newmod,newdata=validation_data[,-10]) #Get the predicitons for the validation set (from the model just fit on the train data)
  
  BC_rand[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use.
}

pred=prediction(BC_rand$holdoutpred,BC_rand$class)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE))) ##AUC = 0.96878
BC_rand$holdoutpred <- factor(BC_rand$holdoutpred, levels = c(1,2), labels = c("benign", "malignant"))
confusionMatrix(BC_rand$holdoutpred,BC_rand$class) ##accuracy = 0.97072

performance <- matrix(c(0.93558,0.9413,0.96878,0.97072),ncol=2,byrow=TRUE)
colnames(performance) <- c("AUC","Accuracy")
rownames(performance) <- c('C5.0','Random forest')
performance <- as.table(performance)
performance
