## Preparing the data
Data <- read.csv("C:\\Users\\Shani\\Downloads\\DATASET.csv", stringsAsFactors = TRUE) #Loading the dataset
str(Data) #looking at the structure of the Data

Data$waterfront <- factor(Data$waterfront, levels = c("0","1")) #converting waterfront into a categorical data
Data$view <- factor(Data$view, levels = c("1","2","3","4")) #converting view into a categorical data
Data$condition <- factor(Data$condition, levels = c("1","2","3","4","5")) #converting condition into a categorical data
Data$is_above_mean_price <- factor(Data$is_above_mean_price, levels = c("0","1")) #converting the is_above_mean_price into a categorical data

Data <- Data[-1] #Removing the ID column since it's redundant
str(Data) #In this new structure you can note the changes we have made. Some variables have been converted into categorical data while the ID column has been removed. We are good to go now!

## Creating normalize function and applying it on the dataset
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
Data_n <- cbind((as.data.frame(lapply(Data[-c(2,8,9,10)], normalize))), Data$is_above_mean_price, Data$waterfront, Data$view, Data$condition) #Applying the normalize function on the numeric variables only and returning the output as a dataframe

summary(Data_n$price) # to confirm that the function was applied correctly
summary(Data_n$floors) # to confirm that the function was applied correctly

## TRAINING THE MODEL ON THE DATA
set.seed(12345)
Data_rand <- Data_n[order(runif(21613)), ]

summary(Data_n$price)
summary(Data_rand$price)

head(Data_n)
head(Data_rand)

0.7 * 21613
Data_rand1 <- Data_rand[-13:-14]
Data_Train <- Data_rand[1:15130, ] ## Splitting the data
Data_Test <- Data_rand[15131:21613, ]

prop.table(table(Data_Train$`Data$is_above_mean_price`))
prop.table(table(Data_Test$`Data$is_above_mean_price`))

install.packages("C50")
library(C50)

Data_Model <- C5.0(Data_Train[-17], Data_Train$`Data$is_above_mean_price`)
Data_Model

summary(Data_Model)

## EVALUATE MODEL PERFORMANCE
Data_Pred <- predict(Data_Model, Data_Test)

install.packages("gmodels")
library(gmodels)
CrossTable(Data_Test$`Data$is_above_mean_price`, Data_Pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('above', 'below'))
