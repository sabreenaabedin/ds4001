## ----  Using Decision Trees  -------------------

## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
credit <- read.csv("C:/Sabreena/Dropbox/DS/credit_PACKT.csv")

credit <- credit_PACKT
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial

set.seed(12345)
credit_rand <- credit[order(runif(1000)), ] #randomize the rows if they're within the data range

# compare the credit and credit_rand data frames
summary(credit$amount)
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)

# split the data frames
credit_train <- credit_rand[1:900, ] #choosing which rows
credit_test  <- credit_rand[901:1000, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree
install.packages("partykit") #partition kit
install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default) #-17 is to take out the outcome
#cs5.0(predictors - everything except row 17, output data - only row 17)

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

# cross tabulation of predicted versus actual classes
install.packages(gmodels)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#73% accurate, add the 57 from (no,no) and 16 from (yes,yes)


### Now let's take a look at multiple class data.
install.packages(RWeka) #go to tools, install packages, click install dependencies when this doesn't work
library(RWeka)
# load data
data(iris)
# fit model
fit <- J48(Species~., data=iris) #
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)

### In the following code, we will use a different type of decision
## tree builder, but it will allow us to create a nice visual
install.packages(party) #had to install manually twice to work
library(party)
str(iris)
iris_ctree <- ctree(Species ~ ., data = iris)
plot(iris_ctree) #expand view fully to see all labels
