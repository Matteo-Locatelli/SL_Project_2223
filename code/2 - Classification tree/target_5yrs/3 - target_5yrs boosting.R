### Use other approaches to perform better ###

# Boosting
# Target: target_5yrs

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library(tree)
library(randomForest)
library(gbm)
library(caret)
library(ellipse)

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

## Pre-processing of data-set
NbaPlayers <- subset(NbaPlayers, select = c(-pts, -reb))

# View(NbaPlayers)
dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
hist(NbaPlayers$target_5yrs)


### Boosting
set.seed(1)

train <- sample(1:nrow(NbaPlayers),floor(nrow(NbaPlayers)*0.5))

# Tune ntrees, depth, shrinkage -> many small trees
ntrees = 5000
boost_model <- gbm(target_5yrs ~ . , data = NbaPlayers[train,], 
                   distribution = "bernoulli" , n.trees = ntrees,
                   interaction.depth = 4, shrinkage = 0.01 , verbose = F)
boost_model
summary(boost_model)
plot(boost_model, i = "gp")

boost_pred <- predict(boost_model, newdata = NbaPlayers[-train,], n.trees = ntrees)
plot(boost_pred, NbaPlayers$target[-train])

train_NbaPlayers = NbaPlayers[train, ]
test_NbaPlayers = NbaPlayers[-train, ]

boxplot(NbaPlayers$gp, main="NbaPlayers gp")
boxplot(NbaPlayers$fg, main="NbaPlayers fg")
boxplot(NbaPlayers$ft, main="NbaPlayers ft")
boxplot(NbaPlayers$min, main="NbaPlayers min")

# boxplot(target ~ gp, data = NbaPlayers, main="NbaPlayers gp")
