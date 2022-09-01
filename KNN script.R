#KNN MODEL EXAMPLE
#Exploring and preparing the data
wdcd <- read.csv("C:/Users/Shani/Downloads/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wdcd)
#removing the first row
wbcd <- wdcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
#normalizing 
normalize <- function(x) {
  return((x- min(x)) / (max(x)- min(x)))
}
normalize(c(1,2,3,4,5))
#z zcore 
normalize1 <- function(x) {
  return((x- mean(x))/ sqrt(var(x)))
}
normalize1(c(1,2,3,4,5))
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

#creating training and test datasets
#split the data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <-wbcd[470:569, 1]

#TRAINING THE MODEL
install.packages("class")
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)

#EVALUATING MODEL PERFORMANCE
install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

#Z-score standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
#divide data into training and test sets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
wbcd_test_pred1 <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21, prob = TRUE)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)
