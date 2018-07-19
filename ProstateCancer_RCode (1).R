setwd("~/Desktop/DS 4559 2016")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors=FALSE)
str(prc)
prc <- prc[-1]
table(prc$diagnosis_result)
prc$diagnosis <- factor(prc$diagnosis_result, levels=c("B","M"), labels = c("Benign", "Malignant"))
round(prop.table(table(prc$diagnosis))*100,digits=1)
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
set.seed(1234)
prc_r <- prc[order(runif(100)),]
prc_n <- as.data.frame(lapply(prc_r[2:8], normalize))
summary(prc_n$radius)


prc_train <- prc_n[1:65,]
prc_test <-prc_n[66:100,]
prc_train_labels <- prc_r[1:65,1]
prc_test_labels <- prc_r[66:100,1]
install.packages("class")
library(class)
prc_test_pred <- knn(train=prc_train,test=prc_test,cl=prc_train_labels,k=10)

## Evaluate

library(gmodels)
CrossTable(x=prc_test_labels, y=prc_test_pred,prop.chisq = FALSE)
prc_test_labels
prc_test_pred
