#Sabreena, Katie, and Maddie

library(sets)
library(DMwR)
library(ggplot2)
data <- read.csv("C:/Users/Sabreena/Dropbox/DS/SpeedDating.csv", header=TRUE, stringsAsFactors=TRUE)

################ CLEAN DATA ################

data[data == ""] <- NA
data[data == " "] <- NA
manyNAs(data, 0.20)
data <- data[-manyNAs(data),]
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

hist(data$race_o)
hist(data$race) 
#are identical, are only of value when the difference between two people is taken


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

#data$success[which(data$success > 0)] <- 1 #use this code to make class attribute binary


##########training and testing split ###########
set.seed(3)
data_rand <- data[order(runif(8092)), ]     
data_train <- data_rand[1:6000, ]      # split testing and training data
data_test  <- data_rand[6001:8092, ]


###########random forest- attribute selection##########
library(randomForest)
r <- randomForest(success ~ ., data_rand)
varImpPlot(r)
varImp(r)

#need under 53 categories

##################### JRIP #######################