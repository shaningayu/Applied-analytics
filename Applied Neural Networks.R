#Modeling the strength of concrete with ANNs
concrete <- read.csv("C:\\Users\\Shani\\Desktop\\Applied analytics\\concrete.csv")
str(concrete)
hist(concrete$strength)
#normalizing
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)

#partition the data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]


#training the model
install.packages("neuralnet")
library(neuralnet)
concrete_model <- neuralnet(strength ~ cement + slag +ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)
plot(concrete_model)

#evaluating model performance
model_results <- compute(concrete_model, concrete_test[1:8])
model_results
#getting the predictions
predicted_strength <- model_results$net.result
predicted_strength
#compare predicted with true values
cor(predicted_strength, concrete_test$strength)

#improving model performance
concrete_model2 <- neuralnet(strength ~ cement + slag +ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5)
plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
