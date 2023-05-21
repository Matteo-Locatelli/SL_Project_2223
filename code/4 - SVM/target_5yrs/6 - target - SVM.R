# Support Vector Machine
# Target: target

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
library(e1071)
set.seed(1) # seed for random number generator

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

## Pre-processing of data-set
NbaPlayers <- subset(NbaPlayers, select = c(-pts, -reb))
#NbaPlayers$target <- factor( ifelse(NbaPlayers$target_5yrs == 0 , " No " , " Yes " ))

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
#hist(NbaPlayers$target_5yrs)

# plot(NbaPlayers, col = NbaPlayers$target_5yrs) -> gives 17^2 plots

# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers), floor(nrow(NbaPlayers)*0.75))

# create the data-frame

x_train = subset(NbaPlayers[train,], select = c(-target_5yrs))
y_train = NbaPlayers[train,]$target_5yrs
dataframe_train <- data.frame(x=x_train, y=as.factor(y_train))

x_test = subset(NbaPlayers[-train,], select = c(-target_5yrs))
y_test = NbaPlayers[-train,]$target_5yrs
dataframe_test <- data.frame(x=x_test, y=as.factor(y_test))


### Fit the SVM with linear kernel / SVC 

## Cross validation for select best value of cost

svm_cross <- tune (svm, y ~ ., data = dataframe_train, kernel = "linear",
                   ranges = list(cost=c(0.001 , 0.01 , 0.1 , 1 , 5 , 10 , 100)))

# get the cross-validation errors for each model
summary(svm_cross)

# access the best model evaluated with cross validation
best_svm <- svm_cross$best.model
summary(best_svm)

## Predict data using best model 

yhat <- predict(best_svm, dataframe_test)
table_svm_cross <- table(predict = yhat, truth = dataframe_test$y)
table_svm_cross

err_rate_svm_cross <- (table_svm_cross[1,2] + table_svm_cross[2, 1]) / sum(table_svm_cross)
err_rate_svm_cross


### Fit the SVM with radial kernel

## tune parameters: cost and gamma
svm_rad_tune <- tune(svm, y ~ ., data = dataframe_train,
                     kernel = "radial",
                     ranges = list (cost = c (0.01 , 0.1 , 1 , 10 , 100),
                                    gamma = c (0.5 , 1 , 2 , 3 , 4)))

summary(svm_rad_tune)

svm_rad_best <- svm_rad_tune$best.model;
summary(svm_rad_best)

# Visualize prediction on test dataset
yhat_rad <- predict(svm_rad_best, dataframe_test)
table_svm_rad <- table(predict = yhat_rad, truth = dataframe_test$y)
table_svm_rad

err_rate_svm_rad <- (table_svm_rad[1,2] + table_svm_rad[2, 1]) / sum(table_svm_rad)
err_rate_svm_rad


### Fit the SVM with polynomial kernel

## tune parameters: cost and gamma
svm_pol_tune <- tune(svm, y ~ ., data = dataframe_train,
                     kernel = "polynomial",
                     ranges = list (cost = c (0.01 , 0.1 , 1 , 10 , 100),
                                    degree = c (0.5 , 1 , 2 , 3 , 4)))

summary(svm_pol_tune)

svm_pol_best <- svm_pol_tune$best.model;
summary(svm_pol_best)

# Visualize prediction on test dataset
yhat_pol <- predict(svm_pol_best, dataframe_test)
table_svm_pol <- table(predict = yhat_pol, truth = dataframe_test$y)
table_svm_pol

err_rate_svm_pol <- (table_svm_pol[1,2] + table_svm_pol[2, 1]) / sum(table_svm_pol)
err_rate_svm_pol
