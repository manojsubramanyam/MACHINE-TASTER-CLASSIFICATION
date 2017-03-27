# Code for implementing Decision trees

rm(list= ls())
# 
# # set working directory
setwd("C:/Data Scientist/Edwisor/Project2/Mini Project/Mini")
# getwd()

# loading packages that are need in this code
library(caret) # for confusion matrix
library(C50) # for decision trees C5.0
library(RWeka) # for C4.5 i.e., J48
#loading training and test data sets from code4
load("Data_traintest.R")


# # # FOR WINE DATA SET BEFORE NORMALIZATION wine_imputed

# Decision tree model with defaults
fit <- C5.0(quality~ . , data= wine_train[-c(12,8,3,1)], method = 'class')
pred <- predict(fit, wine_test[-13], type= 'class')

# building confusion matrix
xtab1 <- table(pred, wine_test[,13])
confusionMatrix(xtab1)



# # # FOR WINE DATA SET AFTER NORMALIZATION wine_norm

# Decision tree model with defaults
fit <- C5.0(quality~ . , data= wine_norm_train[-c(12,1)], method = 'class')
pred <- predict(fit, wine_norm_test[-13], type= 'class')

# building confusion matrix
xtab1 <- table(pred, wine_norm_test[,13])
confusionMatrix(xtab1)
# AUC 57.5


## C4.5 using R weka
# auc 58

# Decision tree model with defaults
fit <- J48(quality~ . , data= wine_train)
pred <- predict(fit, wine_test[-13])

# building confusion matrix
xtab1 <- table(pred, wine_test[,13])
confusionMatrix(xtab1)

# AUC 58


# Decision tree model with defaults
fit <- J48(quality~ . , data= wine_train, control= Weka_control(R = TRUE, M = 5) )
pred <- predict(fit, wine_test[-13])

# building confusion matrix
xtab1 <- table(pred, wine_test[,13])
confusionMatrix(xtab1)

# AUC 54.31 


