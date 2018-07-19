## Here, we examine and practice use of the kNN machine learning algorithm.
## load the data and the working directory (optional)
setwd("C:/Users/Sabreena/Dropbox/DS")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors=FALSE)

## Let's take a look at prc:
str(prc)

## When we examine prc, we realize that the first column is just ID numbers, so we need
## to remove this column:
prc <- prc[-1]

## We also learn, and should keep in mind that the second column has our classifier, "diagnosis_result":
table(prc$diagnosis_result)

## Next we make sure that the diagnosis_result (i.e. the class of the data) is being interpreted
## as a factor.  In addition, we label the classes "Benign" and "Malignant" respectively.
prc$diagnosis <- factor(prc$diagnosis_result, levels=c("B","M"), labels = c("Benign", "Malignant"))

## Next we wish to take a look at what proportion (in terms of percent) of the data is made up 
## of benign cases and malignant cases, respectively.  We are not surprised by the result here,
## as, in this case, we have exactly 100 examples.
round(prop.table(table(prc$diagnosis))*100,digits=1)

## We must ALWAYS normalize data before using the kNN algorithm.  Why?
## Here, we write a function to normalize any vector of variables, x.
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

## Next, we randomize the data to make sure we have a good mix of benign and malignant
## classes in both the training and the testing set:
set.seed(1234)
prc_r <- prc[order(runif(100)),]

## We create a new dataframe in which we have normalized all of the attribute columns of prc_r
prc_n <- as.data.frame(lapply(prc_r[2:8], normalize))

## A quick check that we succeeded at normalization shows a min of 0 and a max of 1, so we
## are confident that we can proceed.
summary(prc_n$radius)

## Now, we divide the data into testin and training data (just the attributes)
prc_train <- prc_n[1:65,]
prc_test <-prc_n[66:100,]

## We make separate vectors for the classes for training and testing that correspond to the 
## matrices above:

prc_train_labels <- prc_r[1:65,1]
prc_test_labels <- prc_r[66:100,1]

## "class" is the package that allows us to perform kNN analysis
install.packages("class")
library(class)

## Here we perform kNN analysis.  The k=10 closest training examples to the test example are found
## and, as their classes are known, they vote as to which class to assign the test example.
## This is completed for all test examples.
prc_test_pred <- knn(train=prc_train,test=prc_test,cl=prc_train_labels,k=10) #10 = sqrt of total data set instances, choose an odd k for a 2 class problem

## Evaluate

library(gmodels)
CrossTable(x=prc_test_labels, y=prc_test_pred,prop.chisq = FALSE)


