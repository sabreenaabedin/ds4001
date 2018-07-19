#Day 3, August 31st - NHIS Data Exploratory Analysis and Cleaning

#what features does the set have?
str(NHISdata)

#what are some general patterns
plot(x = NHISdata$SEX, y = NHISdata$height,
     main = "Scatterplot of Sex vs. Height",
     xlab = "Sex",
     ylab = "height")
plot(x = NHISdata$weight, y = NHISdata$height,
     main = "Scatterplot of Weight vs. Height",
     xlab = "weight",
     ylab = "height")
plot(x = NHISdata$SEX, y = NHISdata$BMI,
     main = "Scatterplot of Sex vs. BMI",
     xlab = "sex",
     ylab = "BMI")

#look for outliers
summary(NHISdata$SEX)
summary(NHISdata$BMI)
summary(NHISdata$height)
summary(NHISdata$weight)

#Begin Classwork
NHIS <- NHISdata
View(NHIS)
hist(NHIS$weight)
install.packages("DWnR")
library(DMnR)
NHIS_sub <- subset(NHIS, weight < 500)
NHIS_sub_males <- subset(NHIS, BMI < 60 & educ < 35 & SEX == 1 & weight < 450 & SLEEP < 25)
summary(NHIS_sub_males)
hist(NHIS_sub_males$SLEEP)

install.packages("ggplot2")
library(ggplot2)
pairs(NHIS_sub_males)
pairs(NHIS_sub_males[,4:9])
