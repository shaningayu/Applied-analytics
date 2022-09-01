#exploring and preparing the data
data_credit<- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/credit.csv" , stringsAsFactors = TRUE)

View(data_credit)
str(data_credit)
data_credit$default <- factor(data_credit$default, levels = c(1,2), labels = c("no", "yes"))
table(data_credit$default)

#randomly ordering the data
set.seed(12345)
data_credit_rand <- data_credit[order(runif(1000)), ]

summary(data_credit$amount)
summary(data_credit_rand$amount)

#to examine the first few values
head(data_credit$amount)
head(data_credit_rand$amount)

#splitting the data
credit_train <- data_credit_rand[1:900, ]
credit_test <- data_credit_rand[901:1000, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

#TRAINING THE MODEL
install.packages("C50")
library(C50)
View(credit_train)
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

#to see the decisions
summary(credit_model)
#The Errors field notes that the model correctly classified all but 101 of the 900 
#training instances for an error rate of 11.2 percent. A total of 29 actual no values were 
#incorrectly classified as yes (false positives), while 72 yes values were misclassified 
#as no (false negatives)

#EVALUATING MODEL PERFORMANCE
#Apply decision tree to tesr dataset 
credit_pred <- predict(credit_model, credit_test)
credit_pred
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
#Out of the 100 test loan application records, our model correctly predicted that 54 
#did not default and 21 did default, resulting in an accuracy of 75 percent and an error 
#rate of 25 percent.

#IMPROVING MODEL PERFORMANCE
#BOOSTING:combining a number of weak performing learners with complementary strengths and weaknesses 
#We simply need to add an additional trials parameter indicating the number of 
#separate decision trees to use in the boosted team. The trials parameter sets an 
#upper limit; the algorithm will stop adding trees if it recognizes that additional trials 
#do not seem to be improving the accuracy

credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10

summary(credit_boost10)
#The classifier made 11 mistakes on 900 training examples for an error rate of 3.3 
#percent

#Applying it to the test data
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
# we reduced the total error rate from 25 percent prior to boosting down to 21 
#percent in the boosted model
# On the other hand, the model is still not doing well at predicting defaults, getting 16 / 32 = 50% wrong, wich is an increase from the prior 11/32 =34.4%

#creating a cost matrix
error_cost <- matrix(c(0,1,4,0), nrow = 2)
error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
#Compared to our best boosted model, this version makes more mistakes overall: 35 
#percent here versus 21 percent in the boosted case
#we have a trade off- increase in false positives (5->30) but decrease in false negatives (16->5)


#k-fold cross validation - XXXXXX
library(caret)
folds <- createFolds(credit$default, k=10)
library(caret)
library(C50)
library(irr)
set.seed(123)
folds <- createFolds(credit$default, k = 10)
cv_results <- lapply(folds, function(x) {
  credit_train <- credit[x, ]
  credit_test <- credit[-x, ]
  credit_model <- C5.0(credit_train[-17], credit_train$default)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})
