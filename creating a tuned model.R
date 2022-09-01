#Creating a simple tuned model
#Loading the data
credit<- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/credit.csv", stringsAsFactors = TRUE)
View(credit)
credit$default <- factor(credit$default, levels = c(1,2), labels = c("no", "yes"))
str(credit)

#tuning the model
install.packages("caret")
library(caret)
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")
m

#applying the best model
p <- predict(m, credit)
table(p, credit$default)

#viewing  predicted classes
head(predict(m, credit))

#viewing predicted probabilities
head(predict(m, credit, type = "prob"))

#CUSTOMIZING THE TUNING PROCESS
#Creating a control object
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")

#creating a grid
grid <- expand.grid(.model = "tree", .trials = c(1, 5, 10, 15, 20, 25, 30, 35), .winnow = "FALSE")
grid

#running the train experiment
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

#USING ENSEMBLES
#BAGGING
install.packages("ipred")
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

#Translating to future performance
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
m1 <-  train(default ~ ., data = credit, method = "treebag", trControl = ctrl)
m1

#RANDOM FORESTS
install.packages("randomForest")
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ . , data = credit)
rf

#COMPARING TO BOOSTED MODEL
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)
#creating the grid
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

#creating the model
set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)

#creating the boosted tree
#creating the grid
grid_c50 <- expand.grid(.model = "tree", .trials = c(10, 20, 30, 40), .winnow = "FALSE")
set.seed(300)
#boosted model
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
               metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_rf
m_c50
