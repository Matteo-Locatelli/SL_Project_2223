
# Classification tree
# Target: target

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library(tree)
set.seed (1)

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

### Complete tree
tree_model_complete <- tree(target ~ . -target_5yrs , NbaPlayers, split = "gini")
tree_model_complete

# show result 
summary(tree_model_complete)
plot(tree_model_complete, type = c("uniform"))
text(tree_model_complete, pretty = 1)


### Evaluate performance with train (fit the model) and test data sets
set.seed(2)
# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers), floor(nrow(NbaPlayers)*0.75))

# tree model with only train data
tree_model <- tree(target ~ . -target_5yrs , NbaPlayers, subset = train)
summary(tree_model)
plot(tree_model, type = c("uniform"))
text(tree_model, pretty = 1)

# predict the values on the test data-set
pred_value <- predict(tree_model, newdata = NbaPlayers[-train,], type = "class")
val_table = table(pred_value, NbaPlayers$target[-train])
val_table
test.err.rate = (val_table[1,2] + val_table[2,1]) / sum(val_table)
test.err.rate

# good tree in training, but bad in validation -> 33% misclassification error rate


### Cross-validation to prune the tree
set.seed(3)
# Input of cv.tree: fitted classification tree and function for pruning
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
tree_cv
plot(tree_cv)

# Number of nodes of each considered tree
tree_cv$size 
# CV error rate
tree_cv$dev 
# Value of the computational cost parameter
tree_cv$k

par(mfrow = c(1,2))
plot(tree_cv$size, tree_cv$dev, type = "b", xlab = "Size", ylab = "Deviance", 
     main = "size-deviance in cross-validation")
plot(tree_cv$k, tree_cv$dev, type = "b", xlab = "k - Penalization factor", ylab = "Deviance", 
     main = "k-deviance in cross-validation")
plot(tree_cv$size, tree_cv$k, type = "b", xlab = "Size", ylab = "k - Penalization factor", 
     main = "size-k in cross-validation")
par(mfrow = c(1,1))

## Try size = 4 for the tree (dev = 338 & k = 2) ##
best1 = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
k1 = min(tree_cv$k[tree_cv$size == best1])

prune1 <- prune.misclass(tree_model, best = best1, k = k1)
summary(prune1)

plot(prune1, type = c("uniform"))
text(prune1, pretty = 0)

pred_value1 <- predict(prune1, newdata = NbaPlayers[-train,], type = "class")
cv_table1 = table(pred_value1, NbaPlayers$target[-train])
cv_table1
test.err.rate.cv1 = (cv_table1[1,2] + cv_table1[2,1]) / sum(cv_table1)
test.err.rate.cv1

# Misclassification error of 30% (still too high), but the tree is simpler

## Try size 6 of the tree (dev = 341 & k = 0) ##
best2 = tree_cv$size[tree_cv$size == 6] 
k2 = min(tree_cv$k[tree_cv$size == best2]) 

prune2 <- prune.misclass(tree_model, best = best2, k = k2)
summary(prune2)

plot(prune2, type = c("uniform"))
text(prune2, pretty = 0)

pred_value2 <- predict(prune2, newdata = NbaPlayers[-train,], type = "class")
cv_table2 = table(pred_value2, NbaPlayers$target[-train])
cv_table2
test.err.rate.cv2 = (cv_table2[1,2] + cv_table2[2,1]) / sum(cv_table2)
test.err.rate.cv2

## Try size 7 of the tree (dev = 341 and dev = -Inf) ###
best3 = tree_cv$size[tree_cv$size == 7] 
k3 = min(tree_cv$k[tree_cv$size == best3]) 

prune3 <- prune.misclass(tree_model, best = best3) # don't specify k = -Inf
summary(prune3)

plot(prune3, type = c("uniform"))
text(prune3, pretty = 0)

pred_value3 <- predict(prune3, newdata = NbaPlayers[-train,],type = "class")
cv_table3 = table(pred_value3, NbaPlayers$target[-train])
cv_table3
test.err.rate.cv3 = (cv_table3[1,2] + cv_table3[2,1]) / sum(cv_table3)
test.err.rate.cv3 

## Try size = 2 for the tree (dev = 345 & k = 18.5) ##
best4 = min(tree_cv$size[tree_cv$size == 2])
k4 = min(tree_cv$k[tree_cv$size == best4])

prune4 <- prune.misclass(tree_model, best = best4, k = k4)
summary(prune4)

plot(prune4, type = c("uniform"))
text(prune4, pretty = 0)

pred_value4 <- predict(prune4, newdata = NbaPlayers[-train,], type = "class")
cv_table4 = table(pred_value4, NbaPlayers$target[-train])
cv_table4
test.err.rate.cv4 = (cv_table4[1,2] + cv_table4[2,1]) / sum(cv_table4)
test.err.rate.cv4

### Tree with size = 4 has least test error (evaluated on the table)

### From the summary, trees with size = 6,7 have same misclassification error 
### rate and it is a bit lower than the tree with size = 4
### Test error rate for size = 4 is the lowest, equal to 0.3 (for other sizes is 0.33)

par(mfrow = c(2,2))
plot(prune1, type = c("uniform"), main="k=4")
text(prune1, pretty = 0)
plot(prune2, type = c("uniform"), main="k=6")
text(prune2, pretty = 0)
plot(prune3, type = c("uniform"), main="k=7")
text(prune3, pretty = 0)
plot(prune4, type = c("uniform"), main="k=2")
text(prune4, pretty = 0)
