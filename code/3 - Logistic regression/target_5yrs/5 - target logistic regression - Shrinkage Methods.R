
# Shrinkage Methods
# Target: target_5yrs

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library(glmnet)
set.seed (1)

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

# Pre-processing of the dataset: removing rebounds
NbaPlayers <- read.csv("./nba_logreg_clean.csv")
NbaPlayers <- subset(NbaPlayers, select = c(-pts, -reb))

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
#hist(NbaPlayers$target_5yrs)


### Shrinkage with full dataset

# Construct Design Matrices and also automatically transforms any qualitative 
# variables into dummy variables
x <- model.matrix ( target_5yrs ~ . , NbaPlayers ) [ , -1]
y <- NbaPlayers$target_5yrs

# lambda grid
lambda <- 10^seq(-3,3,length = 600)
lambda <- c(0,lambda)

set.seed(1)
train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);


## RIDGE REGRESSION

## Ridge
ridge_mod <- glmnet(x, y, family = "binomial",  alpha = 0, lambda = lambda, standardize=TRUE)
dim(coef(ridge_mod)) 
plot(ridge_mod, xvar="lambda")
#ridge_mod
#coef(ridge_mod)

## Ridge CV 

ridge_cv_model <- cv.glmnet(x[train,],y[train], type.measure="class", family = "binomial", 
                            lambda = lambda, alpha = 0, nfolds = 10);
plot(ridge_cv_model)
plot(glmnet(x[train,],y[train], type.measure="class", family = "binomial", 
            lambda = lambda, alpha = 0, nfolds = 10), xvar = "lambda")
ridge_opt_lambda <- ridge_cv_model$lambda.min; # cv_model$lambda.1se
ridge_opt_lambda

## Predict on test dataset with optimal lambda

ridge_model <- glmnet(x[train,], y[train], family = "binomial", 
                      alpha = 0, lambda = ridge_opt_lambda, standardize=TRUE)
ridge_fitt_value <- predict(ridge_model, newx = x[-train,], type="response")

ridge_predictions = rep(0, dim(NbaPlayers)[1])
ridge_predictions[ridge_fitt_value > .5] = 1
ridge_table = table(ridge_predictions, NbaPlayers$target_5yrs)
ridge_table

ridge_mer = (ridge_table[1,2] + ridge_table[2,1]) / sum(ridge_table)
ridge_mer


## LASSO REGRESSION

## Lasso

lasso_mod <- glmnet( x, y, family = "binomial", alpha = 1, lambda = lambda, standardize=TRUE)
dim(coef(ridge_mod)) 
plot(lasso_mod, xvar="lambda")
#lasso_mod
#coef(lasso_mod)

## Lasso CV

lasso_cv_model <- cv.glmnet(x[train,], y[train], type.measure="class", family = "binomial",
                      lambda = lambda, alpha=1, nfolds = 10);
plot(lasso_cv_model)
plot(glmnet(x[train,],y[train], type.measure="class", family = "binomial", 
            lambda = lambda, alpha = 1, nfolds = 10), xvar = "lambda")
lasso_opt_lambda <- lasso_cv_model$lambda.min #lasso_cv_model$lambda.1se
lasso_opt_lambda

## Predict on test dataset with optimal lambda

lasso_model <- glmnet(x[train,], y[train], family = "binomial",
                      alpha = 1, lambda = lasso_opt_lambda, standardize=TRUE)
lasso_fitt_value <- predict(lasso_model, newx = x[-train,], type="response")

lasso_predictions = rep(0, dim(NbaPlayers)[1])
lasso_predictions[lasso_fitt_value > .5] = 1
lasso_table = table(lasso_predictions, NbaPlayers$target_5yrs)
lasso_table

lasso_mer = (lasso_table[1,2] + lasso_table[2,1]) / sum(lasso_table)
lasso_mer
