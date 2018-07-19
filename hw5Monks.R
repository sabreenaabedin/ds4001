############### PROBLEM 1 ###################
## setup

data <- Monks
library(e1071)
library(caret)
library(lattice)
library(ggplot2)
library(gmodels)
library(C50)
data$V1 <- as.factor(data$V1)

### TRIAL 1
## randomize 
set.seed(1)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb1<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c1 <- (46+37)/83     #from cross table


### TRIAL 2
## randomize 
set.seed(2)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb2<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c2 <- (39+19)/83     #from cross table


### TRIAL 3
## randomize 
set.seed(3)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb3<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c3 <- (42+22)/83     #from cross table


### TRIAL 4
## randomize 
set.seed(4)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb4<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c4 <- (38+23)/83     #from cross table


### TRIAL 5
## randomize 
set.seed(5)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb5<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c5 <- (41+22)/83     #from cross table


### TRIAL 6
## randomize 
set.seed(6)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb6<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c6 <- (40+22)/83    #from cross table


### TRIAL 7
## randomize 
set.seed(7)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb7<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c7 <- (36+22)/83     #from cross table


### TRIAL 8
## randomize 
set.seed(8)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb8<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c8 <- (52+13)/83     #from cross table


### TRIAL 9
## randomize 
set.seed(9)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb9<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c9 <- (36+27)/83     #from cross table


### TRIAL 10
## randomize 
set.seed(10)
data_rand <- data[order(runif(432)), ]
d_train <- data_rand[1:350,]
d_test <- data_rand[350:432,]

## Naive Bayes 
modelposteriorprob <- naiveBayes(V1 ~ ., data = d_train, type = "raw")
model <- naiveBayes(V1 ~ ., data = d_train)
result <- predict(model, d_test)
nb10<-confusionMatrix(result, d_test$V1)

#C5.0 
tree <- C5.0(d_train[-1], d_train$V1)
pred <- predict(tree, d_test) #test model
CrossTable(d_test$V1, pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))
c10 <- (43+20)/83     #from cross table


## COMPARE
nb1
nb2
nb3
nb4
nb5
nb6
nb7
nb8
nb9
nb10
temp <- as.data.frame(c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10))
temp["naive bayes"] <- (c(0.6386, 0.6265, 0.7108, 0.7108, 0.7349, 0.6746, 0.9277, 0.6627, 0.6867, 0.6265)) #from nb_ results
colnames(temp) <- c('ctree','nbayes')
plot(temp)


############### PROBLEM 3 ###################

load(PimaIndiansDiabetes)
d <- PimaIndiansDiabetes

## Use an optimization routine (hill-climbing in this case) to determine network structure.
res <- hc(d)
plot(res)
## Remove arcs that don't make sense
res$arcs <- res$arcs[-which((res$arcs[,'to'] == "age")),]
res
## Create conditional tables for each node
res
fittedbn <- bn.fit(res, data = d)


## Make inferences
cpquery(fittedbn, event = (diabetes =="pos"), evidence = ( age < "50"))
#0.3330401, low chance of having diabetes given that the person is under age 50

cpquery(fittedbn, event = (diabetes =="neg"), evidence = ( pressure > "80" & mass < "40"))
#0.6609071, low chance of having diabetes (high chance of a negative result), given a 
#pressure of greater than 80 and a mass of under 40

cpquery(fittedbn, event = (pregnant >"2"), evidence = ( age < "24"))
#0.4629535, relatively low chance of more than two kids if under the age of 24
