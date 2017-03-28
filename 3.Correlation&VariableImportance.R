# In this code, we discuss correlation matrix and correlation plots and variable importance plots using random forest

# clear workspace
rm(list= ls())

# # set working directory
setwd("C:/Data Scientist/Edwisor/Project2/Mini Project/Mini")
getwd()

# loading necessary packages
library(clusterSim) # for normalization techniques
library(randomForest) # for variable importance check using random forest
library(corrplot) # for correlation matrix
library(ggcorrplot) # for plotting correlation matrix using ggplot and corrplot
library(usdm) # for checking variance inflation factor

# loading Data structure from previous code
load(file = "DataWine2.R")

#Normalizing Data 
wine_norm <- data.Normalization(wine_imputed[-c(12, 13)], type = "n4", normalization = "column")


# Checking Correlation between variables.
# We use correlation matrix and correlation plots for checking correlations

# correlation plots
corrplot(cor(wine_norm), order= "hclust")

# using ggcorrplot package for more details``
ggcorrplot(cor(wine_norm), hc.order = TRUE, type = "lower",
           lab = TRUE, title = "Correlation plot using ggcorrplot")

# this shows correlation between alcohol[11] and density[8]; free SO2[6] and Total SO2[7]


# spearman correlation matrix is used here as data is not normally distributed
cor(wine_norm, use = "complete.obs", method= "spearman")



# checking variable imporntance using random forest
# changes red to '0' and white to '1' for simpler representation
wine_norm$type <- factor(wine_imputed$type, labels= 0:(length(levels(wine_imputed$type))-1))
wine_norm$quality <- wine_imputed$quality

var_imp <- randomForest(quality~. , data= wine_norm, ntree= 500, importance= T)
importance(var_imp,type=1)

varImpPlot(var_imp, color = '#0d2f66')

# density[8] is less important than alcohol
# total SO2[7] is less important than free S02 in explaining variance
# fixed acidity is representing low gini value and
# being less important in explaining variance


# Checking Variance Inflation Factor
vif(wine_norm[-c(12,13)])


# saving all data structures for next code 
save(list=c('red','white','wine_imputed','wine_norm') ,file= "DataWine3.R")
