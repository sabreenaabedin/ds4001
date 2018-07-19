#-----------library packages and import data------------
library(DMwR)
library(ggplot2)
#install.packages("data.table")
library(data.table)

#import data and add column names from information file
cd <- read.csv("C:/Users/Sabreena/Dropbox/DS/CloudData1.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(cd) <- c('mean','max','min','mean.distr','contrast','entropy','ang.momentum','IR.mean','IR.max','IR.min')

cd[cd == "?"] <- NA

#make numeric so that data can be analyzed
cd = transform(cd, min = as.numeric(cd$min), max = as.numeric(cd$max), mean = as.numeric(cd$mean), mean.distr = as.numeric(cd$mean.distr))
cd = transform(cd, contrast = as.numeric(cd$contrast), entropy = as.numeric(cd$entropy), ang.momentum = as.numeric(cd$ang.momentum), IR.mean = as.numeric(cd$IR.mean))
cd = transform(cd, IR.max = as.numeric(cd$IR.max), IR.min = as.numeric(cd$IR.min))

#---------determine what might be considered an outlier---------
View(cd)
summary(cd)

#go back and swtich min and mean because the columns are reversed in the data set
colnames(cd) <- c('min','max','mean','mean.distr','contrast','entropy','ang.momentum','IR.min','IR.max','IR.mean')

boxplot(cd$min) #above 75
boxplot(cd$max) #none
boxplot(cd$mean) #above 150
boxplot(cd$mean.distr) #remove max value above .16
boxplot(cd$contrast) #above 2700
boxplot(cd$entropy)#none
boxplot(cd$ang.momentum)#remove max value above 600
boxplot(cd$IR.min) #below 25
boxplot(cd$IR.max) #below 200
boxplot(cd$IR.mean) #below 140

#---------replace any unusable values/outliers with NA---------
sum(is.na(cd))

outlierReplace = function(dataframe, cols, rows, newValue = NA){
  if(any(rows)){
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(cd,"min", which(cd$min > 75), NA)
sum(is.na(cd))
outlierReplace(cd,"mean", which(cd$mean > 150), NA)
sum(is.na(cd))
outlierReplace(cd,"contrast", which(cd$contrast > 2700), NA)
sum(is.na(cd))
outlierReplace(cd,"IR.min", which(cd$IR.min < 25), NA)
sum(is.na(cd))
outlierReplace(cd,"IR.max", which(cd$IR.max < 200), NA)
sum(is.na(cd))
outlierReplace(cd,"IR.mean", which(cd$IR.mean < 140), NA)
sum(is.na(cd))
outlierReplace(cd,"mean.distr", which(cd$mean.distr > .16), NA)
sum(is.na(cd))
outlierReplace(cd,"ang.momentum", which(cd$ang.momentum > 600), NA)
sum(is.na(cd))

#---------begin imputing values---------

manyNAs(cd,0.2)
cd <- cd[-manyNAs(cd),]
nrow(cd)
sum(is.na(cd))

# first try: correlation imputation
symnum(cor(cd, use = "complete.obs"))
#mean and IR mean: 0.95
#mean.distr and contrast: 0.95

which(is.na(cd$mean)) #how many mean values can be imputated 
lm(mean~IR.mean,data=cd)

cd[1,"mean"]<- 346.346+(-1.368)*cd[1,"IR.mean"]
cd[55,"mean"]<- 346.346+(-1.368)*cd[55,"IR.mean"]
cd[124,"mean"]<- 346.346+(-1.368)*cd[124,"IR.mean"]
cd[308,"mean"]<- 346.346+(-1.368)*cd[308,"IR.mean"]
cd[324,"mean"]<- 346.346+(-1.368)*cd[324,"IR.mean"]
cd[401,"mean"]<- 346.346+(-1.368)*cd[401,"IR.mean"]
cd[586,"mean"]<- 346.346+(-1.368)*cd[586,"IR.mean"]
cd[704,"mean"]<- 346.346+(-1.368)*cd[704,"IR.mean"]
sum(is.na(cd))

which(is.na(cd$IR.mean)) #which values can be filled in using reverse of this correlation
lm(IR.mean~mean,data=cd)

cd[166,"IR.mean"]<- 249.3414+(-0.6362)*cd[166,"mean"]
cd[370,"IR.mean"]<- 249.3414+(-0.6362)*cd[370,"mean"]
cd[426,"IR.mean"]<- 249.3414+(-0.6362)*cd[426,"mean"]
cd[791,"IR.mean"]<- 249.3414+(-0.6362)*cd[791,"mean"]

sum(is.na(cd))


which(is.na(cd$mean.distr)) # which mean.distr values can be imputated 
lm(mean.distr~contrast,data=cd)

cd[158,"mean.distr"]<- 1.848e-02+-7.129e-05*cd[158,"contrast"]
cd[267,"mean.distr"]<- 1.848e-02+-7.129e-05*cd[267,"contrast"]
cd[549,"mean.distr"]<- 1.848e-02+-7.129e-05*cd[549,"contrast"]
cd[608,"mean.distr"]<- 1.848e-02+-7.129e-05*cd[608,"contrast"]
cd[673,"mean.distr"]<- 1.848e-02+-7.129e-05*cd[673,"contrast"]
cd[758,"mean.distr"]<- 1.848e-02+-7.129e-05*cd[758,"contrast"]

sum(is.na(cd))

which(is.na(cd$contrast)) # which NAs which can be filled in using reverse correlation
lm(contrast~mean.distr,data=cd)

cd[135,"contrast"]<- -151.3+12215.6*cd[135,"mean.distr"]
cd[158,"contrast"]<- -151.3+12215.6*cd[158,"mean.distr"]
cd[571,"contrast"]<- -151.3+12215.6*cd[571,"mean.distr"]
cd[646,"contrast"]<- -151.3+12215.6*cd[646,"mean.distr"]
cd[673,"contrast"]<- -151.3+12215.6*cd[673,"mean.distr"]

sum(is.na(cd))

#determine where missing values still exist

sum(is.na(cd$min)) #2
sum(is.na(cd$max)) #3
sum(is.na(cd$mean)) #0
sum(is.na(cd$mean.distr)) #2
sum(is.na(cd$contrast)) #2
sum(is.na(cd$entropy)) #3
sum(is.na(cd$ang.momentum)) #5
sum(is.na(cd$IR.max)) #7
sum(is.na(cd$IR.min)) #3
sum(is.na(cd$IR.mean)) #0

#---------normalize data ---------
norm = function(value) {
  normalized = (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
  print(normalized)
}

newmax <- norm(cd$max)
newmean <- norm(cd$mean)
newmean.distr <- norm(cd$mean.distr)
newcontrast <- norm(cd$contrast)
newentropy <- norm(cd$entropy)
newang.momentum <- norm(cd$ang.momentum)
newIRmin <- norm(cd$IR.min)
newIRmax <- norm(cd$IR.max)
newIRmean <- norm(cd$IR.mean)

cd$min <- newmin
cd$max <- newmax
cd$mean <- newmean
cd$mean.distr <- newmean.distr
cd$contrast  <- newcontrast
cd$entropy <- newentropy
cd$ang.momentum <- newang.momentum
cd$IR.min <- newIRmin
cd$IR.max <- newIRmax
cd$IR.mean <- newIRmean

#begin knnImputation
cd<- knnImputation(cd, k=32)

sum(is.na(cd)) #check that all NAs are gone

hist(cd$min)
hist(cd$max)
hist(cd$mean)
hist(cd$mean.distr)
hist(cd$entropy)
hist(cd$contrast)
hist(cd$ang.momentum)
hist(cd$IR.min)
hist(cd$IR.max)
hist(cd$IR.mean)



write.csv(cd, file = "CloudDataFinal.csv")
