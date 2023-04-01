
# Classification tree
# Target: target_5yrs

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library(tree)
library (ISLR2)
set.seed (1)

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

### Complete tree
tree_model <- tree( target ~ . -target_5yrs , NbaPlayers, split = "gini")

# show result 
summary(tree_model)
plot(tree_model)
text(tree_model,pretty = 1)


### Valuate performance with train (fit the model) and test data sets
set.seed(2)
# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers),floor(nrow(NbaPlayers)*0.75))

# tree model with only train data
tree_model <- tree( target ~ . -target_5yrs , NbaPlayers, subset = train)
summary(tree_model)
plot(tree_model)
text(tree_model,pretty = 1)

# predict the values on the test datas-et
pred_value <- predict(tree_model, newdata = NbaPlayers[-train,], type = "class")

table(pred_value, NbaPlayers$target_5yrs[-train])

# good tree in training, but bad in validation -> 32% misclassification error


### Cross validation 
set.seed(2)
# Input of cv.tree: fitted classification tree and function for pruning
tree_cv <- cv.tree(tree_model, FUN = prune.misclass)
tree_cv

# deviance: quantity to assess the quality of the tree.
plot(tree_cv$size, tree_cv$dev)

# Minimum of the tree -> compare optimal dimension of the tree
tree_cv$dev
min(tree_cv$dev)
best = min(tree_cv$size[tree_cv$dev == min(tree_cv$dev)])
best
# penalize the complexity of the tree
k = min(tree_cv$k[tree_cv$dev == min(tree_cv$dev)]) #alpha in the book
k

prune <- prune.misclass(tree_model, best = best)
summary(prune)

plot(prune)
text(prune, pretty = 0)

pred_value <- predict(prune, newdata = NbaPlayers[-train,], type = "class")
table(pred_value, NbaPlayers$target[-train])

# Same misclassification error (32%), but tree is simpler


### Use other approaches to perform better ###

### Bagging


### Random Forest


### Boosting
