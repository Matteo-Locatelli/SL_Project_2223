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
                   distribution = "bernoulli" , n.trees = ntrees, cv.folds = 10,
                   interaction.depth = 5, shrinkage = 0.001 , verbose = F)
boost_model

# Plot influence of each variable
summary(boost_model)
#plot(boost_model, i = "gp")

best_iter = gbm.perf(boost_model, method="cv")
summary(boost_model, n.trees = best_iter)
print(pretty.gbm.tree(boost_model, i.tree = best_iter))
#plot.gbm(boost_model, "gp", best_iter)

boost_pred <- predict.gbm(boost_model, newdata = NbaPlayers[-train,], 
                          n.trees = ntrees, type = "response")
boost_pred <- round(boost_pred)
plot(boost_pred, NbaPlayers$target_5yrs[-train])
boost_table = table(boost_pred, NbaPlayers$target_5yrs[-train]) 
boost_table

# Column types as categorical factors (not numeric)
cm = confusionMatrix(reference = as.factor(NbaPlayers$target_5yrs[-train]), 
                     data = as.factor(boost_pred))

## Plots 

boxplot(NbaPlayers$gp, main="NbaPlayers gp")
boxplot(NbaPlayers$fg, main="NbaPlayers fg")
boxplot(NbaPlayers$ft, main="NbaPlayers ft")
boxplot(NbaPlayers$min, main="NbaPlayers min")

# boxplot(target ~ gp, data = NbaPlayers, main="NbaPlayers gp")


### Compute classification error rate

threshold = 0.5

result = data.frame(NbaPlayers[-train,]$target_5yrs, boost_pred)
# print(result)

pred_with_th = ifelse(boost_pred < threshold , 0 , 1 )
# print(pred_with_th)

result_with_th = data.frame(NbaPlayers[-train,]$target_5yrs, pred_with_th)
# print(result_with_th)

y_neg_pred_neg = sum(NbaPlayers[-train,]$target_5yrs == 0 & pred_with_th == 0)
y_neg_pred_pos = sum(NbaPlayers[-train,]$target_5yrs == 0 & pred_with_th == 1)
y_pos_pred_neg = sum(NbaPlayers[-train,]$target_5yrs == 1 & pred_with_th == 0)
y_pos_pred_pos = sum(NbaPlayers[-train,]$target_5yrs == 1 & pred_with_th == 1)
sum = y_neg_pred_neg + y_neg_pred_pos + y_pos_pred_neg + y_pos_pred_pos

# Misclassification error rare
err = (y_neg_pred_pos + y_pos_pred_neg)/(sum)
err

