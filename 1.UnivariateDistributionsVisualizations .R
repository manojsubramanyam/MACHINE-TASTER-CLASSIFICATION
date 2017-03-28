# In this code, we load data sets and analyze Data distribution and Outliers 
rm(list= ls())

# setting working directory
setwd("C:/Data Scientist/Edwisor/Project2/Mini Project/Mini")
getwd()


# loading packages used in this analysis
library(reshape2)
library(ggplot2)
library(clusterSim)
library(dplyr)


# loading data sets
red <- read.csv(file="winequality-red.csv", sep= ";")
white <- read.csv(file= "winequality-white.csv", sep= ";")

red$type <- factor("red")
white$type <- factor("white")
red$quality <- factor(red$quality)
white$quality <- factor(white$quality)

# creating master data set wine
wine <- rbind(red,white)
wine <- wine[,c(1:11,13,12)]
# checking the data types and structure
head(wine)
str(wine)

#summary of wine master dataset
summary(wine)


# checking variable distributions using graphical representations.
dev.off() # to reset ggplot2 settings and clear graphics memory

# for Fixed acidity
ggplot(data= wine,  aes(x= fixed.acidity, fill=type)) +
  geom_histogram(binwidth=0.5, aes(fill= type))+
  labs(title= "Fixed Acidity Histogram", x= "Fixed Acidity Distribution")

# for Volatile acidity
ggplot(data= wine,  aes(x= volatile.acidity, fill=type)) +
  geom_histogram(binwidth=0.05, aes(fill= type))+
  labs(title= "Volatile Acidity Histogram", x= "Volatile Acidity Distribution")

# for Citric acid
ggplot(data= wine,  aes(x= citric.acid, fill=type)) +
  geom_histogram(binwidth=0.05, aes(fill= type))+
  labs(title= "Citric Acid Histogram", x= "Citric Acid Distribution")

# for Residual sugar
ggplot(data= wine,  aes(x= residual.sugar, fill=type)) +
  geom_histogram(binwidth=1, aes(fill= type))+
  labs(title= "Residual Sugar Histogram", x= "Residual Sugar Distribution")

# for Chlorides
ggplot(data= wine,  aes(x= chlorides, fill=type)) +
  geom_histogram(binwidth=0.01, aes(fill= type))+
  labs(title= "Chlorides Histogram", x= "Chlorides Distribution")

# for Free Sulfur Dioxide
ggplot(data= wine,  aes(x= free.sulfur.dioxide, fill=type)) +
  geom_histogram(binwidth= 5, aes(fill= type))+
  labs(title= "Free Sulfur Dioxide Histogram", x= "Free Sulfur Dioxide Distribution")


# for Total Sulfur Dioxide
ggplot(data= wine,  aes(x= total.sulfur.dioxide, fill=type)) +
  geom_histogram(binwidth= 10, aes(fill= type))+
  labs(title= "Total Sulfur Dioxide Histogram", x= "Total Sulfur Dioxide Distribution")

# for Density
ggplot(data= wine,  aes(x= density, fill=type)) +
  geom_histogram(binwidth=0.002, aes(fill= type))+
  labs(title= "Density Histogram", x= "Density Distribution")

# for pH
ggplot(data= wine,  aes(x= pH, fill=type)) +
  geom_histogram(binwidth=0.05, aes(fill= type))+
  labs(title= "pH Histogram", x= "pH Distribution")

# for Sulphates
ggplot(data= wine,  aes(x= sulphates, fill=type)) +
  geom_histogram(binwidth=0.05, aes(fill= type))+
  labs(title= "Sulphates Histogram", x= "Sulphates Distribution")

# for Alcohol
ggplot(data= wine,  aes(x= alcohol, fill=type)) +
  geom_histogram(binwidth=0.5, aes(fill= type))+
  labs(title= "Alcohol Histogram", x= "Alcohol Distribution")

# for Quality
ggplot(data= wine,  aes(x= (quality), fill=type)) +
  geom_bar(aes(fill= type))+
  labs(title= "Quality Bar graph", x= "Quality Distribution")+scale_y_sqrt()


# Check for Outliers

# creating melt dataframe of wine for checking outliers

# taking out quality and type for normalization
# Normalizing data to get boxplot in same range

wine_norm <- data.Normalization(wine[-c(12,13)], type = "n4", normalization = "column")
wine_norm$type <- wine$type # adding type back after normalization

melt.wine <- melt(wine_norm, id.vars = "type")

# plotting outliers 

ggplot(data <- melt.wine, aes(x= variable, y= value))+ geom_boxplot(aes(fill= type))+
  labs(title <- "Boxplot and data point distribution of all Variables of Master data set w.r.t 'type'  variable", x= "Variables", y= "Range")+
  facet_grid( ~ variable, scales="free")




# saving the data structure to use in other scripts
save(list=c('red','white','wine'), file= "DataWine1.R")
