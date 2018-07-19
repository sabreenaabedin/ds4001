data <- concrete
library(neuralnet)

#normalize and randomize, split into test and train

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
data <- as.data.frame(lapply(data[1:8], normalize))
set.seed(3)
data_rand <- data[order(runif(1029)), ] 
data_train <- data_rand[0:700,]
data_test <- data_rand[701:1029,]

#m<-neuralnet(target ~predictors, data = mydata, hidden = 1)
d <- data_train
model <- neuralnet(d$strength ~ d$cement, data = d, hidden = 1)
#p<- compute(m,test)

#test
#strength_predictions <-p$net.result
