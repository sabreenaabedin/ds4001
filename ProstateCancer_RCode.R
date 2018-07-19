setwd("C:/Users/Sabreena/Dropbox/DS")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors=FALSE)
str(prc)
prc <- prc[-1] #remove the id column because it's useless as an attribute
table(prc$diagnosis_result)
prc$diagnosis <- factor(prc$diagnosis_result, levels=c("B","M"), labels = c("Benign", "Malignant")) #gives factors a labeled name
round(prop.table(table(prc$diagnosis))*100,digits=1) #gives you percentage if you didn't have exactly 100 instances
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
prc_n <- as.data.frame(lapply(prc[2:9], normalize)) #apply normalize function to every column in prc from 2-9
summary(prc_n$radius)
prc_train <- prc_n[1:65,]
prc_test <-prc_n[66:100,]
prc_train_labels <- prc[1:65,1]
prc_test_labels <- prc[66:100,1]
install.packages("class")
library(class)
prc_test_pred <- knn(train=prc_train,test=prc_test,cl=prc_train_labels,k=10) #don't make model, go right into test - lazy model
## Evaluate

library(gmodels)
CrossTable(x=prc_test_labels, y=prc_test_pred,prop.chisq = FALSE)
