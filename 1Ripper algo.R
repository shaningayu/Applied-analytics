mushrooms <- read.csv("C:\\Users\\Shani\\Downloads\\mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

#removing veil type
mushrooms$veil_type <- NULL

table(mushrooms$type)
levels(mushrooms$type) <- c("edible", "poisonous")

#training the model
library(RWeka)
install.packages("RWeka")
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

#evaluating model performance
summary(mushroom_1R)

#improving model performance
library(RWeka)
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
