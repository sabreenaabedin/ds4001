#sabreena abedin, katie murray, and maddy morales
#iris model example
library(RWeka)
data(iris)
summary(iris)
set.seed(12345)
iris_rand <- iris[order(runif(150)), ]

iris_train <- iris_rand[1:110,]
iris_test <- iris_rand[111:150,]
m <- OneR(Species ~ ., data = iris_train)
p <- predict(m, iris_test) #m = name of model
m

#start in class mushroom example
#import mushrooms.csv
md <- mushrooms
View(md)
type <- factor(c("edible", "poisonous"))

set.seed(12345)
m_rand <-md[order(runif(8124)),]
m_train <- m_rand[1:6093,]
m_test <- m_rand[6094,8124]

#feature selection:
correlationMatrix <- cor(m_train[]) #not numeric, can't use correlation matrix 

#can use randomForest because it can handle mixed data types
library(randomForest)
attr <- randomForest(type ~ ., mushrooms) #missing a package, fix later
attr
importance(attr) #gives numeric importance based on GINI
varImpPlot(attr) #visualizes, shows odor as important

#i never actually removed any attributes

# OneR - can call mixed data, doesn't need discrete or numeric specifically
oneRMushroom <- OneR(type ~ ., data = m_train)
predictOneR <- predict(oneRMushroom, m_test)
oneRMushroom #view results

#C5.0
library(C50)
mushroom_model <- C5.0(m_train[-1], m_train$type) #remove type (row 1) from first argument 
mushroom_model #made a tree of 0, try something else
#too much data for C5.0 to handle 

#cart modeling
library(rpart)
#rpart(type ~ , data = m_train, method= class)  #~. means that type is predicted by rest of column

#c tree
library(party)
library(ggplot2)
library(caret)

Treemodel = ctree(type~.,data=m_train) #try c tree
plot(Treemodel)
p <- predict(Treemodel,m_test)
confusionMatrix(p,m_test$type)
crossTable()

m2 <- OneR(type ~ ., data = m_train) #try OneR
p2 <- predict(m2, m_test)
m2
confusionMatrix(p2,m_test$type)

m3 <- JRip(type~.,data=m_train) #try JRip
p3 <- predict(m3, m_test)
m3
confusionMatrix(p3,m_test$type)
#JRip can use non-numerical data


#rerandomize
set.seed(7)
m_rand <-md[order(runif(8124)),]
m_train <- m_rand[1:6093,]
m_test <- m_rand[6094,8124]

m4 <- JRip(type~.,data=m_train)
p4 <- predict(m4, m_test)
m4
confusionMatrix(p4,m_test$type)

# the rules aren't perfect so we may want to consider a tree that uses more attributes to further break down edible vs poisionous 
# we would rather have false poisionous calls than being told that something edible would kill someone
# the type should be poisonous and non poisonous rather than edible so that you know if you can't eat it but should still be cautious if it isn't
# make a smaller dataset for the mushrooms in your area for a more precise prediction
# odor is the most important attribute

sum(is.na(md)) #no nas for confusion matrix error
