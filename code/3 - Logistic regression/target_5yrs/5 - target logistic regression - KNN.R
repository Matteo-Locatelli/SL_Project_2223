# K Nearest Neighbors 
# Target: target

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
library(class)
set.seed(1) # seed for random number generator

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

## Pre-processing of data-set
NbaPlayers <- subset(NbaPlayers, select = c(-pts, -reb))
NbaPlayers$target <- factor( ifelse(NbaPlayers$target_5yrs == 0 , " No " , " Yes " ))

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
#hist(NbaPlayers$target_5yrs)

# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers), floor(nrow(NbaPlayers)*0.75))

matrix <- NbaPlayers[train,]
data_train <- subset(NbaPlayers[train,], select = c(-target_5yrs, -target)) 
data_test <- subset(NbaPlayers[-train,], select = c(-target_5yrs, -target))
label_train <- NbaPlayers$target[train]; 


knn_fit <- knn(data_train,data_test,label_train, k=3)

table_knn <- table(knn_fit, NbaPlayers$target[-train])
table_knn

err_rate_knn = (table_knn[1,2] + table_knn[2,1]) / sum(table_knn)
err_rate_knn
