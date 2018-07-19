#load data
data = read.csv("C:/Users/Sabreena/Dropbox/DS/IndonesionContraception.csv")
View(data)

#part a: no NAs in data
sum(is.na(data)) 
data <- data[,-1]

#part b:categorical as factors
data$wife_ed <- factor(data$wife_ed, levels=c("1","2","3","4"))
data$husband_ed <- factor(data$husband_ed, levels=c("1","2","3","4"))
data$husband_cc <- factor(data$husband_cc, levels=c("1","2","3","4"))
data$SOL_index <- factor(data$SOL_index, levels=c("1","2","3","4"))
data$contraception <- factor(data$contraception, levels=c("0", "1"))

#part c: create training and test sets
set.seed(2)
data_rand<- data[order(runif(1473)),]

data_train <- data_rand[1:1105,] #75% training, 25% testing
data_test <-data_rand[1105:1473,]

#part d: create c5.0 and ctree and compare similarity and accuracy
  #check the proportion of class variable
  prop.table(table(data_train$contraception))
  prop.table(table(data_test$contraception)) #close, can continue
  
  #create C5.0 tree
  library(C50)
  data_model <- C5.0(data_train[-10], data_train$contraception) #10 is class attribute
  data_model
  plot(data_model)
  
  
  data_pred <- predict(data_model, data_test) #check accuracy
  library(gmodels)
  CrossTable(data_test$contraception, data_pred,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual contraception', 'predicted contraception'))
  accuracy5.0 = (83+169)/369
  
  #create ctree
  library(party)
  data_ctree <- ctree(contraception ~ ., data = data_train)
  plot(data_ctree)
  data_ctree
  
  data_pred2 <- predict(data_ctree, data_test) #check accuracy
  CrossTable(data_test$contraception, data_pred2,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual contraception', 'predicted contraception'))
  accuracyctree = (87+171)/369
  
  #compare
  accuracy5.0
  accuracyctree
  #ctree and c5.0 have almost identical splits and have accuracies that differ by 0.01 

#part e: feature selection- pick top 3 features
library(randomForest)
library(mlbench)
library(caret)

r <- randomForest(contraception ~ ., data_rand)
varImpPlot(r)
varImp(r)  #most important 3 are wife age, wife education, and number of children


#part f: try c5.0 with reduced set
data_sub_train = data_train[,c(1,2,4,10) ]
View(data_sub_train)
data_sub_test = data_test[,c(1,2,4,10)]
data_model_red <- C5.0(data_sub_train[-4], data_sub_train$contraception) #4 is class attribute
data_model_red
plot(data_model_red)


data_pred_red <- predict(data_model_red, data_sub_test) #check accuracy
library(gmodels)
CrossTable(data_sub_test$contraception, data_pred_red,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual contraception', 'predicted contraception'))
accuracy5.0_new = (90+174)/369

#part g: write rules from tree
 #see screenshot, didn't complete due to time
 #ex. if number of children is less than 0, contraception = 0
