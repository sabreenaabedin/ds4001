################## PROBLEM 1 ######################

## set up
library(neuralnet)
data <- read.csv("C:/Users/Sabreena/Dropbox/DS/BloodTransfusion.txt", header=TRUE, stringsAsFactors=TRUE)
colnames(data) <- c("recency","frequency", "money", "time", "donatedMarch")
data$donatedMarch <- factor(c(0,1))
levels(data$donatedMarch) <- c('yes', 'no')

## The data attempts to predict who is most likely to donate blood in order to maximize their marketing efforts

## splitting up training and testing data
set.seed(1)
size.sample <- 200
datatrain <- data[sample(1:nrow(data), size.sample),] 
nnet_train <- datatrain

nnet_train <- cbind(nnet_train, datatrain$donatedMarch == '1')
nnet_train <- cbind(nnet_train, datatrain$donatedMarch == '0')
names(nnet_train)[6] <- 'yes'
names(nnet_train)[7] <- 'no'

## create model
nn <- neuralnet(yes+no ~ 
                  recency + frequency + money + time,
                data=nnet_train, 
                hidden=c(3))
plot(nn)

## predict
mypredict <- compute(nn, data[,-5])$net.result
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)
prediction <- c('yes', 'no')[idx]

## results
table(prediction, data$donatedMarch)  ## FIX THIS

###################### PROBLEM 2 ###########################

data <- read.csv("C:/Users/Sabreena/Dropbox/DS/WisconsinBC.txt", header=TRUE, stringsAsFactors=TRUE)
View(data)






