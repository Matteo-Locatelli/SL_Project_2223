
# Classification tree
# Target: target_5yrs

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
tree_model_complete <- tree( target ~ . -target_5yrs , NbaPlayers, split = "gini")

# show result 
summary(tree_model_complete)
plot(tree_model_complete)
text(tree_model_complete, pretty = 1)


### Evaluate performance with train (fit the model) and test data sets
set.seed(2)
# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers),floor(nrow(NbaPlayers)*0.75))

# tree model with only train data
tree_model <- tree( target ~ . -target_5yrs , NbaPlayers, subset = train)
summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 1)

# predict the values on the test data-set
pred_value <- predict(tree_model, newdata = NbaPlayers[-train,], type = "class")
val_table = table(pred_value, NbaPlayers$target_5yrs[-train])
val_table
test.err.rate = (val_table[1,2] + val_table[2,1]) / sum(val_table)
test.err.rate

# good tree in training, but bad in validation -> 32% misclassification error


### Cross-validation to prune the tree
set.seed(2)
# Input of cv.tree: fitted classification tree and function for pruning
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
tree_cv
plot(tree_cv)

plot(tree_cv$size, tree_cv$dev)

tree_cv$size 
tree_cv$dev # 320 (for size of 4 and 6) & 321 (for size 7)
tree_cv$k

## Try size = 4 for the tree (dev = 320 & k = 2) ##
best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
# k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)]) # k=0 for size = 6
k = min(tree_cv$k[tree_cv$size == best])

prune <- prune.misclass(tree_model, best = best, k = k)
summary(prune)

plot(prune, type = c("uniform"))
text(prune, pretty = 0)

pred_value <- predict(prune, newdata = NbaPlayers[-train,], type = "class")
cv_table = table(pred_value, NbaPlayers$target[-train])
cv_table
test.err.rate.cv = (cv_table[1,2] + cv_table[2,1]) / sum(cv_table)
test.err.rate.cv

# Misclassification error of 30% (still too high), but the tree is simpler

## Try size 6 of the tree (dev = 320 & k = 0) ##
best2 = tree_cv$size[tree_cv$size == 6] 
k2 = min(tree_cv$k[tree_cv$size == best2]) 

prune2 <- prune.misclass(tree_model, best = best2, k = k2)
summary(prune2)

plot(prune2, type = c("uniform"))
text(prune2, pretty = 0)

pred_value2 <- predict(prune2, newdata = NbaPlayers[-train,],type = "class")
cv_table2 = table(pred_value2, NbaPlayers$target[-train])
test.err.rate.cv2 = (cv_table2[1,2] + cv_table2[2,1]) / sum(cv_table2)
test.err.rate.cv2

## Try size 7 of the tree (dev = 321 and dev = -Inf) ###
best3 = tree_cv$size[tree_cv$size == 7] 
k3 = min(tree_cv$k[tree_cv$size == best3]) 
prune3 <- prune.misclass(tree_model, best = best3) # don't specify k
summary(prune3)

plot(prune3)
text(prune3, pretty = 0)

pred_value3 <- predict(prune3, newdata = NbaPlayers[-train,],type = "class")
cv_table3 = table(pred_value3, NbaPlayers$target[-train])
test.err.rate.cv3 = (cv_table3[1,2] + cv_table3[2,1]) / sum(cv_table3)
test.err.rate.cv3 

### Simplest tree has least misclassification error evaluated on the table
### From the summary trees with size = 6,7 have same misclassification error rare
### and it is a bit lower than the tree with size = 4
