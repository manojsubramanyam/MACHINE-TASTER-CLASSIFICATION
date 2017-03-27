# This code implements Ensemble models namely Random forest.

rm(list= ls())
# 
# # set working directory
setwd("C:/Data Scientist/Edwisor/Project2/Mini Project/Mini")
# getwd()

# loading packages that are need in this code
library(caret) # for confusion matrix
library(randomForest) # for random forest modelling



#loading training and test data sets from code4
load("Data_traintest.R")



# # # FOR WINE DATA SET BEFORE NORMALIZATION wine_imputed

# random forest model
rf_model <- randomForest(quality~. , data= wine_train[-c(12,8)], ntree= 500)
pred <- predict(rf_model, wine_test[-13])

xtab1 <- table(pred, wine_test[,13])
confusionMatrix(xtab1)

# save(rf_model, file = 'rf_model70_08.rda')
# save(rf_model, file= 'rf_model70_15.rda')



# # # FOR WINE DATA SET AFTER NORMALIZATION wine_norm

# random forest model
rf_model <- randomForest(quality~. , data= wine_norm_train[-c(12,8)], ntree= 500)
pred <- predict(rf_model, wine_norm_test[-13])

xtab2 <- table(pred, wine_norm_test[,13])
confusionMatrix(xtab2)
