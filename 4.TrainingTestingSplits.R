# Preparing train and test data sets for wine_norm and wine_imputed
rm(list= ls())
# 
# # set working directory
setwd("C:/Data Scientist/Edwisor/Project2/Mini Project/Mini")
# getwd()

# loading packages that are need in this code
library(caTools) # for sampling 

# loading data structures of wine
load("DataWine3.R")
set.seed(123)
spl <- sample.split(wine_norm$quality, SplitRatio = 0.8)

# Train and test sets for Unnormalized data set wine_imputed
wine_train <- subset(wine_imputed, spl== T)
wine_test <- subset(wine_imputed, spl== F)

# Train and test sets for Normalized data set wine_norm
wine_norm_train <- subset(wine_norm, spl== T)
wine_norm_test <- subset(wine_norm, spl== F)


# saving data structures of training and test data sets for further usuage
save(list=c('wine_train','wine_test','wine_norm_train','wine_norm_test'), file = "Data_traintest.R")
