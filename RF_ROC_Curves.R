## Load data and libraries
usedcars <- read.csv('~/Desktop/DS 4559 2016/usedcars.csv')
library(randomForest)
library(ROCR)

## Create randomForest model
rf_output=randomForest(x=usedcars[,1:5], y=usedcars[,6], importance = TRUE, ntree = 1000, proximity=TRUE)
rf_output

## Calculate predictions in terms of votes (percentage of trees that voted for particular class)
predictions=as.vector(rf_output$votes[,2])
## Pred is comparing numerical percentages to binary responses.  This is what we need to create ROC curve
pred=prediction(predictions,usedcars[,6])

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

## Explore with me;
perf_ROC
perf_AUC


################mushrooms data ###################
data <- mushrooms
rf_output=randomForest(x=data[,-1], y=data[,1], importance = TRUE, ntree = 1000, proximity=TRUE)
rf_output

predictions=as.vector(rf_output$votes[,2])
## Pred is comparing numerical percentages to binary responses.  This is what we need to create ROC curve
pred=prediction(predictions,data[,1]) #classifier

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

## Explore with me;
perf_ROC
perf_AUC
