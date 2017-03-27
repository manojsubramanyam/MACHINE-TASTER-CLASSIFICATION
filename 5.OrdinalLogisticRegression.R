# Code for implementing ordinal logistic regression, the base model

rm(list= ls())
# 
# # set working directory
setwd("C:/Data Scientist/Edwisor/Project2/Mini Project/Mini")
# getwd()

# loading packages that are need in this code
library(caret) # for confusion matrix
library(MASS) # for ordinal logistic regression

#loading training and test data sets from code4
load("Data_traintest.R")

# Building Logistic regression models

# # # FOR WINE DATA SET BEFORE NORMALIZATION wine_imputed
# # Logistic regression model
# default and base model
lr_model <- polr(quality ~. , data = wine_train[-c(12,8,1,3)], Hess=TRUE, method= "logistic")
pred <- predict(lr_model, wine_test[-13])

# building confusion matrix
xtab1 <- table(pred, wine_test[,13])
confusionMatrix(xtab1)

# Ordinal regression with probit method
pr_model <- polr(quality ~. , data = wine_train[-c(12,8,1,3)], Hess=TRUE, method= "probit")
pred <- predict(pr_model, wine_test[-13])

# building confusion matrix
xtab2 <- table(pred, wine_test[,13])
confusionMatrix(xtab2)



# # # FOR WINE DATA SET AFTER NORMALIZATION wine_norm
# # Logistic regression model
# default and base model
lr_model <- polr(quality ~. , data = wine_norm_train[-c(12,8,1,3)], Hess=TRUE, method= "logistic")
pred <- predict(lr_model, wine_norm_test[-13])

# building confusion matrix
xtab1 <- table(pred, wine_norm_test[,13])
confusionMatrix(xtab1)

# Ordinal regression with probit method
pr_model <- polr(quality ~. , data = wine_norm_train[-c(12,8,1,3)], Hess=TRUE, method= "probit")
pred <- predict(pr_model, wine_norm_test[-13])

# building confusion matrix
xtab2 <- table(pred, wine_norm_test[,13])
confusionMatrix(xtab2)
