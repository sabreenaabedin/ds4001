################## PROBLEM 1 ######################

## set up
library(neuralnet)
data <- read.csv("~/Data Science/data/BloodTransfusion.txt", header=TRUE, stringsAsFactors=TRUE)
colnames(data) <- c("recency","frequency", "money", "time", "donatedMarch")
data$donatedMarch <- factor(c(0,1))
levels(data$donatedMarch) <- c('no', 'yes')

## The data attempts to predict who is most likely to donate blood in order to maximize their marketing efforts
##based on information about past donors


## splitting up training and testing data
set.seed(1)
size.sample <- 200
datatrain <- data[sample(1:nrow(data), size.sample),] 
nnet_train <- datatrain

nnet_train <- cbind(nnet_train, datatrain$donatedMarch == 'no')
nnet_train <- cbind(nnet_train, datatrain$donatedMarch == 'yes')
names(nnet_train)[6] <- 'no'
names(nnet_train)[7] <- 'yes'

## create model
nn <- neuralnet(no+yes ~ 
                  recency + frequency + money + time,
                data=nnet_train, 
                hidden=c(1))
plot(nn)

## predict
mypredict <- compute(nn, data[,-5])$net.result
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c('no', 'yes')[idx]

## results
confusionMatrix(prediction, data$donatedMarch)  ## accuracy = 0.5000 w/ 1 hidden node

nodes <- c(1,2,3,4,5,6,7,8,9,10)
accuracy <-c(0.5000, 0.5000, 0.5160, 0.5000,0.5000, 0.5107, 0.5107,0.4933, 0.4946,0.4946)

plot(nodes,accuracy)
##best performance was accuracy=0.5160 for 1 hidden layer with three nodes









