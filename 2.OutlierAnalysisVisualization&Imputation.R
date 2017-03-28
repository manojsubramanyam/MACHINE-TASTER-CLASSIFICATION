# Outlier analysis, visualization and Imputation

rm(list= ls())

# set working directory
setwd("C:/Data Scientist/Edwisor/Project2/Mini Project/Mini")
getwd()

# loading packages that are necessary in this code
library(dplyr)
library(ggplot2)
library(DMwR)

# loading the data structures created in code1.R
load("DataWine1.R")

# outlier analysis on wine

# considering outliers count for each variable
wine_out <- NULL
wine_out <- data.frame(var= character(), outlier_number= character(), stringsAsFactors = F)
for ( i in 1:ncol(wine[1:11])){
  val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
  wine_out= rbind(wine_out, data.frame(colnames(wine[i]),length(val),round((length(val)/ length(wine[,i]))*100, digits=2 )))
  row.names(wine_out)= NULL
}
names(wine_out) <- c("Variables","Outliers","Percentage")

# wine_out gives us the outlier information 
wine_out
# Visualizing Outliers using bargraph

ggplot(wine_out, aes(x=Variables, y=Outliers))+ geom_col(colour="blue", fill= "#ef5b5b")+
  labs(title= "Variable-wise Outliers proportion")


# Not all outliers are out of normal range, so we impute those outliers who are located out of range based on Wine compositions

# Based on Domain understanding, we replace outliers with NAs


for ( i in 1:ncol(wine[1:11])){
  if(i==1){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & (wine[,i]<1 | wine[,i]>12)] = NA
  }
  if(i==2){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & wine[,i]>1.2] = NA
  }
  if(i==3){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & wine[,i]>0.5] = NA
  }
  if(i==4){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & (wine[,i]<0.2 | wine[,i]>15)] = NA
  }
  if(i==5){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & wine[,i]>0.156] = NA
  }
  if(i==6){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & wine[,i]>112.5] = NA
  }
  if(i==7){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & wine[,i]>175] = NA
  }
  if(i==8){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val] = NA
  }
  if(i==9){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val ] = NA #pH outliers are already out of range
  }
  if(i==10){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val] = NA
  }
  if(i==11){
    val= wine[,i][wine[,i] %in% boxplot.stats(wine[,i])$out]
    wine[,i][wine[,i] %in% val & (wine[,i])<9 | wine[,i]>16] = NA
  }
}


# Imputation of NAs in wine
# checking mean, median, knnimputation methods

# a= wine
# a[1,6] # value is 11
# a[1,6]=NA
# mean(a[,6],na.rm = T) # gives 30.38
# median(a[,6],na.rm = T) # gives 29
# a=knnImputation(a)
# a[1,6] # gives 12.9469 with knn


# mean and median are checked for imputation and they are proved to be futile

# Imputing with knnImputation

wine_imputed <- knnImputation(wine)



# saving the data structures for further script files
save(list=c('red','white','wine_imputed'),file="DataWine2.R")
