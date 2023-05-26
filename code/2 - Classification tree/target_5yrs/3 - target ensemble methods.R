### Use other approaches to perform better ###

# Ensemble methods
# Target: target

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
NbaPlayers$target <- factor( ifelse(NbaPlayers$target_5yrs == 0 , " No " , " Yes " ))

# View(NbaPlayers)
dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
hist(NbaPlayers$target_5yrs)


### Bagging
set.seed(1)

train <- sample(1:nrow(NbaPlayers),floor(nrow(NbaPlayers)*0.75))

bagg_model <- randomForest(target ~ . -target_5yrs ,data = NbaPlayers, subset = train,
                           mtry = ncol(NbaPlayers) - 2, ntree = 500, 
                           importance = TRUE, replace = TRUE)
bagg_model
summary(bagg_model)
plot(bagg_model, main = "Bagging model error")
legend("topright", colnames(bagg_model$err.rate), col=1:3, cex=0.75, fill=1:3)
bagg_model$confusion

# compute prediction and compare it with test data in the data-set
bagg_pred <- predict(bagg_model, newdata = NbaPlayers[-train,])
plot(bagg_pred, NbaPlayers$target[-train], main = "Prediction of bagging", xlab = "", ylab = "")
bagg_table = table(bagg_pred, NbaPlayers$target[-train]) 
bagg_table
bagging.test.err.rate = (bagg_table[1,2] + bagg_table[2,1])/ sum(bagg_table)
bagging.test.err.rate

importance(bagg_model)
varImpPlot(bagg_model, main = "Bagging model importance")

# vector error rates of the prediction on the input data, the i-th element being 
# the (OOB) error rate for all trees up to the i-th
bagg_length = length(bagg_model$err.rate) # 600 for some reason
bagg_model$err.rate[bagg_length]


### Random Forest

forest_model <- randomForest(target ~ . -target_5yrs ,data = NbaPlayers, subset = train,
                             mtry = floor(sqrt(ncol(NbaPlayers) - 2)), ntree =500, 
                             importance = TRUE)
forest_model
summary(forest_model)
forest_model$confusion
plot(forest_model, main = "Random forest model error")
legend("topright", colnames(forest_model$err.rate), col=1:3, cex=0.75, fill=1:3)

# compute prediction and compare it with test data in the data-set
rf_pred <- predict(forest_model, newdata = NbaPlayers[-train,])
plot(rf_pred, NbaPlayers$target[-train], main = "Prediction of random forest", xlab = "", ylab = "")
rf_table = table(rf_pred, NbaPlayers$target[-train]) 
rf_table
rf.test.err.rate = (rf_table[1,2] + rf_table[2,1])/ sum(rf_table)
rf.test.err.rate

importance(forest_model)
varImpPlot(forest_model, main = "Random forest model importance")

rf_length = length(forest_model$err.rate) # 600 for some reason
forest_model$err.rate[rf_length]

## Look for the best m
# Out_Of_Bag error & test error rates
oob.err.rate <- c(0,0)
test.err.rate <- c(0,0)

for(mtry in 1:(ncol(NbaPlayers) - 2)){
  fit = randomForest(target ~ . -target_5yrs, data = NbaPlayers, subset=train, 
                     mtry=mtry, ntree = 500, importance = TRUE)
  oob.err.rate[mtry] = fit$err.rate[600]
  yhat <- predict(fit, newdata = NbaPlayers[-train,])
  fit_table = table(yhat, NbaPlayers$target[-train])
  test.err.rate[mtry] = (fit_table[1,2] + fit_table[2,1]) / sum(fit_table)
}

# plot the results
plot(oob.err.rate, type='b', main="Random Forest (m)",
     ylab = "error rate", xlab = "m", 
     col = 'blue', pch=18, lwd = 3,
     ylim = range(c(oob.err.rate, test.err.rate)))
par(new=TRUE)
lines(test.err.rate, type = 'b', main = "Random Forest (m)",
     ylab = "error rate", xlab = "m", 
     col = 'red', pch=18, lwd=3)
legend("topright", legend=c("oob", "test"), 
       col=c("blue", "red"), pch = 18)
abline(v = which.min(test.err.rate), col="green", lwd = 3)


## Compare random forest models
oob.err.rate[which.min(oob.err.rate)]
test.err.rate[which.min(oob.err.rate)]
oob.err.rate[which.min(test.err.rate)]
test.err.rate[which.min(test.err.rate)]


## Best with m = 1 ##
best_m = which.min(test.err.rate)

best_rf <- randomForest(target ~ . -target_5yrs, data = NbaPlayers, subset = train,
                             mtry = best_m, ntree = 500, importance = TRUE)
best_rf
summary(best_rf)
best_rf$confusion
plot(best_rf, main = "Random forest model error (m=1)")
legend("topright", colnames(best_rf$err.rate), col=1:3, cex=0.75, fill=1:3)

# compute prediction and compare it with test data in the data-set
best_rf_pred <- predict(best_rf, newdata = NbaPlayers[-train,])
plot(best_rf_pred, NbaPlayers$target[-train],  main = "Prediction of random forest (m=5)", 
     xlab = "", ylab = "")
best_rf_table = table(best_rf_pred, NbaPlayers$target[-train]) 
best_rf_table
best.rf.test.err.rate = (best_rf_table[1,2] + best_rf_table[2,1])/ sum(best_rf_table)
best.rf.test.err.rate

importance(best_rf)
varImpPlot(best_rf, main = "Random forest model importance (m=1)")

length = length(best_rf$err.rate) # 600 for some reason
best_rf$err.rate[length]


### Compare bagging and random forest
plot(bagg_model, type = 'b', col="green", pch = "+", lwd = 2, 
     ylim = range(c(bagg_model$err.rate, best_rf$err.rate)),
     main = "bagging vs random forest")
par(new=TRUE)
plot(best_rf, type = 'b', col = "red", pch = 'o', lwd = 2, axes = FALSE,
     main = "bagging vs random forest")
legend("topright", legend=c("bag", "rf"), 
       col=c("green", "red"), pch = c("+", "o"), cex = 0.75)


### Boosting
set.seed(3)

# Tune ntrees, depth, shrinkage -> many small trees
ntrees = 5000
boost_model <- gbm(target_5yrs ~ . -target, data = NbaPlayers[train,], 
                   distribution = "bernoulli", n.trees = ntrees, cv.folds = 10,
                   interaction.depth = 4, shrinkage = 0.001 , verbose = F)
boost_model
summary(boost_model)
#plot(boost_model, i="gp")

# cv.error=green, train.error=black, best_iteration=blue
best_iter = gbm.perf(boost_model, method="cv", plot.it = TRUE)
legend("topright", legend=c("cv.error", "train.error"), 
       col=c("green", "black"), pch = c(15, 15), cex = 1)
summary(boost_model, n.trees = best_iter)
print(pretty.gbm.tree(boost_model, i.tree = best_iter))
#plot.gbm(boost_model, "gp", best_iter)

boost_pred <- predict.gbm(boost_model, newdata = NbaPlayers[-train,], 
                          n.trees = ntrees, type = "response")
boost_pred <- round(boost_pred)

plot(boost_pred, NbaPlayers$target[-train])
boost_table = table(boost_pred, NbaPlayers$target_5yrs[-train]) 
boost_table

# Column types as categorical factors (not numeric)
cm = confusionMatrix(reference = as.factor(NbaPlayers$target_5yrs[-train]), 
                     data = as.factor(boost_pred))
cm

## Various plots ### USELESS

train_NbaPlayers = NbaPlayers[train, ]
test_NbaPlayers = NbaPlayers[-train, ]

featurePlot(x = train_NbaPlayers[, c("gp", "fg")], 
            y = train_NbaPlayers$target,
            plot = "density", 
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(1, 2), 
            auto.key = list(columns = 2))

featurePlot(x = train_NbaPlayers[, c("gp", "fg")], 
            y = train_NbaPlayers$target, 
            plot = "ellipse",
            auto.key = list(columns = 2))

boxplot(NbaPlayers$gp, main="NbaPlayers gp")
boxplot(NbaPlayers$fg, main="NbaPlayers fg")
boxplot(NbaPlayers$ft, main="NbaPlayers ft")
boxplot(NbaPlayers$min, main="NbaPlayers min")

# boxplot(target ~ gp, data = NbaPlayers, main="NbaPlayers gp")


### Compute boosting classification error rate

threshold = 0.5

result = data.frame(NbaPlayers[-train,]$target_5yrs, boost_pred)
# print(result)

# Put the values of boost_pred between 0 and 1, then threshold
pred_with_th = ifelse(boost_pred - min(boost_pred) < threshold , 0 , 1 )
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

