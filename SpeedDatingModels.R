#Sabreena, Katie, and Maddy

library(caret)
library(sets)
library(DMwR)
library(ggplot2)
data <- read.csv("C:/Users/Sabreena/Dropbox/DS/SpeedDating.csv", header=TRUE, stringsAsFactors=TRUE)

################ CLEAN DATA ################

data[data == ""] <- NA
data[data == " "] <- NA
data <- data[-manyNAs(data, .20),]
sum(is.na(data))


sum(is.na(data$attr4_1)) 
sum(is.na(data$fun4_1))
sum(is.na(datat$amb4_1))

#since these are missing 1/4 the values, we removed section due to inconsistent data

data$attr4_1 <- NULL
data$sinc4_1 <- NULL
data$intel4_1 <- NULL
data$fun4_1 <- NULL
data$amb4_1 <- NULL
data$shar4_1 <- NULL

#we decided that survey 1 rankings were most interesting in relation to each other but didn't have enough data to make it useful
#deleted this section of surveys in the excel sheet and reloaded

data <- read.csv("C:/Users/Sabreena/Dropbox/DS/SpeedDatingNew.csv", header=TRUE, stringsAsFactors=TRUE)

data[data == ""] <- NA
data[data == " "] <- NA
data <- data[-manyNAs(data, 0.20),]

#factor categorical data
data$gender <- factor(data$gender, levels=c("0","1"), labels = c("F", "M")) 
data$condtn <- factor(data$condtn, levels=c("1","2"), labels = c("limited", "extensive"))
data$match <- factor(data$match, levels=c("1","0"), labels = c("Y", "N"))
data$race <- factor(data$race)
data$goal <- factor(data$goal)
data$go_out <- factor(data$go_out)
data$date_3 <- factor(data$date_3, levels=c("1","2"), labels = c("Y", "N"))
data$date_3[is.na(data$date_3)] <- "N"

data$date <- as.numeric(data$date)
data$age_o <- as.numeric(data$age_o)
data$pf_o_att <- as.numeric(data$pf_o_att)
data$pf_o_sin <- as.numeric(data$pf_o_sin)
data$pf_o_int <- as.numeric(data$pf_o_int)
data$pf_o_fun <- as.numeric(data$pf_o_fun)
data$pf_o_amb <- as.numeric(data$pf_o_amb)
data$pf_o_sha <- as.numeric(data$pf_o_sha)

##for later class attribute calculation##
data$you_call[is.na(data$you_call)] <- 0
data$them_cal[is.na(data$them_cal)] <- 0
data$numdat_3[is.na(data$numdat_3)] <- 0
data$num_in_3[is.na(data$num_in_3)] <- 0


###### MEAN IMPUTATION ########
#using mean to impute categorical data - skewed due to repeats of same person in larger waves
for (j in 1:length(data[1,]))
{
  if (is.numeric(data[,j]))
  {
    for(k in 1:length(data[,1]))
    {
      if(is.na(data[k,j]))
      {
        data[k,j] <- mean(data[,j],na.rm=T)
      }
    }
  }
}


#insert correlation imputation in the future

sapply(data, function(x) sum(is.na(x))) #check for more NAs
sum(is.na(data))


############# ALTERNATE VARIABLES ######################

data$samerace <- factor(data$samerace, levels=c("0","1"), labels = c("Diff", "Same"))

#age
comp <- as.data.frame(as.numeric(data$age))
comp["other"] <- as.numeric(data$age_o)

colnames(comp) <- c('person','other')
comp["diff"] <- (comp$person- comp$other)
comp$diff[which(comp$diff < 3)] <- 0 # if within 3 years, similar ages
comp$diff[which(comp$diff >= 3)] <- 1
comp$diff <- factor(comp$diff, levels=c("0","1"), labels = c("Same", "Diff"))

data["sameage"] <- NA 
data$sameage <- comp$diff



################ CALCULATE CLASS ATTRIBUTE ################
data$you_call <- as.numeric(data$you_call)
data$them_cal <- as.numeric(data$them_cal)
data$numdat_3 <- as.numeric(data$numdat_3)
data$num_in_3 <- as.numeric(data$num_in_3)

temp <- as.data.frame(c(data$you_call))
temp["them_cal"] <- data$them_cal
temp["numdat_3"] <- data$numdat_3
temp["num_in_3"] <- data$num_in_3
temp<-transform(temp, sum=rowSums(temp))

data["success"] <- NA 
data$success <- temp$sum
data$success[which(data$success > 0)] <- 1 #use this code to make class attribute binary
data$success <- factor(data$success, levels=c("0","1"), labels = c("N", "Y"))

sum(is.na(data$success))
#have a binary outcome "success" for all 7895 instances (no NAs)

##########training and testing split ###########
set.seed(3)
data_rand <- data[order(runif(7895)), ] 
forModels <- subset(data_rand[,-(50:54)]) #remove attributes used to calc success
forModels <- subset(forModels[,-2])
data_train <- forModels[1:5920, ]      # split testing and training data
data_test  <- forModels[5921:7895, ]


###########random forest- attribute selection##########
library(randomForest)
r <- randomForest(success ~ ., data_train) #if using to subset data, only use train set
varImpPlot(r)
head(data_train$match_es)


##################### JRIP #######################
library(RWeka)
ripper <- JRip(success~.,data=data_train)
ripper
summary(ripper)

pRip <-predict(ripper,data_test)
confusionMatrix(pRip,data_test$success)

####################ctree########################
library(party)
ctreeModel = ctree(success~.,data=data_train)
plot(ctreeModel)

p <- predict(ctreeModel,data_test)
confusionMatrix(p,data_test$success)

###################oneR###########################
oneRule <- OneR(success ~ ., data = data_train)
oneRule
pRule <- predict(oneRule, data_test)
confusionMatrix(pRule,data_test$success)
