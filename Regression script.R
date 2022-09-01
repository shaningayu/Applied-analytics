#LINEAR REGRESSION
#STEP 1: LOAD THE DATA
insurance <- read.csv("C:\\Users\\Shani\\Desktop\\Applied analytics\\insurance.csv", stringsAsFactors = TRUE)
str(insurance)
#Exploring distrubution and relationships between variables
summary(insurance$charges)
hist(insurance$charges)
table(insurance$region)
cor(insurance[c("age", "bmi", "children", "charges")])
pairs(insurance[c("age", "bmi", "children", "charges")])
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

#STEP 2: ESTIMATING THE LINEAR MODEL
ins_model <- lm( charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model
summary(ins_model)

#adding a non-linear term
insurance$age2 <- insurance$age^2
ins_model1 <- lm( charges ~ age + age2 + children + bmi + sex + smoker + region, data = insurance)
summary(ins_model1)

#converting a numeric variable to a binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm( charges ~ age + children + bmi + bmi30 + sex + smoker + region, data = insurance)
summary(ins_model2)

#adding interaction effects
ins_model3 <- lm( charges ~ age + children + bmi + bmi30*smoker + sex + region, data = insurance)
summary(ins_model3)

#putting everything together
ins_model4 <- lm( charges ~ age + age2 + children + bmi + bmi30*smoker + sex + region, data = insurance)
summary(ins_model4)
