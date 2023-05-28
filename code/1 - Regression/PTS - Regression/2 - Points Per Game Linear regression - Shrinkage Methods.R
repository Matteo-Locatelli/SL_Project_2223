
# Shrinkage Methods
# Target: Point Scored Per Game

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library(glmnet)
set.seed (1)

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

# Pre-processing of the dataset: removing rebounds
NbaPlayers <- read.csv("./nba_logreg_clean.csv")
NbaPlayers <- subset(NbaPlayers, select = c(-reb))

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)


### Shrinkage with full dataset

## RIDGE REGRESSION

# Construct Design Matrices and also automatically transforms any qualitative 
# variables into dummy variables
x <- model.matrix ( pts ~ . , NbaPlayers ) [ , -1]
y <- NbaPlayers$pts

# -3, 3, 400 -> plot of ridge coefficients vs lambda
# -3, 3, 400 -> plot of lasso coefficients vs lambda
# -3, 3, 1000 -> evaluate lambda optimal -> between -3 and -1
# -3, -1, 400 -> values of lambda and test_MSE
# lambda grid
lambda <- 10^seq(-3,-1,length = 400)
lambda <- c(0,lambda)

train <- sample(dim(x)[1],floor(dim(x)[1]*0.75),replace = FALSE);


## Ridge
ridge_mod <- glmnet(x, y, alpha = 0,lambda = lambda, standardize=TRUE)
dim(coef(ridge_mod)) 
plot(ridge_mod)
#ridge_mod
#coef(ridge_mod)

## ridge cv 
ridge_cv_model <- cv.glmnet(x[train, ],y[train], lambda = lambda, alpha = 0, nfolds = 10);
plot(ridge_cv_model)
ridge_opt_lambda <- ridge_cv_model$lambda.min; # cv_model$lambda.1se

# predict on test dataset
ridge_model <- glmnet(x[train,],y[train],alpha = 0,lambda = ridge_opt_lambda,standardize=TRUE)
ridge_model$dev.ratio
rideg_fitt_value <- predict(ridge_model,newx = x[-train,])
ridge_train_MSE = mean((y[train] - predict(ridge_model,newx = x[train,]))^2)
ridge_train_MSE
ridge_test_MSE = mean((y[-train] - rideg_fitt_value)^2)
ridge_test_MSE
ridge_MSE = mean((y - predict(ridge_model,newx = x))^2)
ridge_MSE


## Lasso
lasso_mod <- glmnet( x[train , ] , y[ train ], alpha = 1,lambda = lambda)
dim(coef(ridge_mod)) 
plot(lasso_mod)
#lasso_mod
#coef(lasso_mod)

# lasso cv
cv_lasso <- cv.glmnet(x[train,],y[train],lambda = lambda, alpha=1,nfolds = 10);
plot(cv_lasso)
lasso_opt_lambda <- cv_lasso$lambda.min #cv_lasso$lambda.1se

# predict on test dataset
lasso_model <- glmnet(x[train,],y[train],alpha = 1,lambda = lasso_opt_lambda,standardize=TRUE)
lasso_model$dev.ratio
lasso_fitt_value <- predict(lasso_model,newx = x[-train,])
lasso_train_MSE = mean((y[train] - predict(lasso_model,newx = x[train,]))^2)
lasso_train_MSE
lasso_test_MSE = mean((y[-train] - lasso_fitt_value)^2)
lasso_test_MSE

### Shrinkage with variables omitting

y <- array(unlist(NbaPlayers[3]))
all_regressors <- colnames(NbaPlayers[-3])

r_squared_array <- rep(1:length(all_regressors))
mse <- rep(1:length(all_regressors))
j <- 1
for(i in 1:dim(NbaPlayers)[2]){
  if(i != 3){
    x <- array(unlist(NbaPlayers[i]))
    lm_fit <- lm(y ~ x)
    r_squared_array[ j ] <- summary(lm_fit)$r.squared
    mse[ j ] <- summary(lm_fit)$sigma
    j <- j + 1
  }
}

thresholds = c(0.9,0.8)

ridge_optimal_lambdas <- c(0,0)
lasso_optimal_lambdas <- c(0,0)
ridge_optimal_cv_models <- vector(mode = "list", length = 2)
lasso_optimal_cv_models <- vector(mode = "list", length = 2)
ridge_optimal_models <- vector(mode = "list", length = 2)
lasso_optimal_models <- vector(mode = "list", length = 2)
ridge_optimal_lm <- vector(mode = "list", length = 2)
lasso_optimal_lm <- vector(mode = "list", length = 2)
ridge_test_mse_values <- c(0,0)
lasso_test_mse_values <- c(0,0)

for(i in 1:2){
  colSelection <- c(all_regressors[r_squared_array <= thresholds[i]], "pts")
  SubNbaPlayers <- NbaPlayers[colSelection]
  pts_index <- dim(SubNbaPlayers)[2]
  
  x <- model.matrix ( pts ~ . , SubNbaPlayers) [ , -1]
  y <- SubNbaPlayers$pts
  
  # Ridge
  ridge_cv_model <- cv.glmnet(x[train, ],y[train], lambda = lambda, alpha = 0, nfolds = 10);
  ridge_model <- glmnet(x[train,],y[train],alpha = 0,lambda = ridge_cv_model$lambda.min,standardize=TRUE)
  ridge_fitt_value <- predict(ridge_model,newx = x[-train,])
  
  SubNbaPlayers_ridge <- SubNbaPlayers[c(array(unlist(predict(ridge_model,type="nonzero"))), pts_index)]
  
  ridge_test_mse_values[i] <- mean((y[-train] - ridge_fitt_value)^2)
  ridge_optimal_lambdas[i] <- ridge_cv_model$lambda.min;
  ridge_optimal_cv_models[[i]] <- ridge_cv_model
  ridge_optimal_models[[i]] <- ridge_model
  ridge_optimal_lm[[i]] <- lm(pts ~ ., data = SubNbaPlayers_ridge[train,])
  
  # Lasso
  lasso_cv_model <- cv.glmnet(x[train,],y[train],lambda = lambda, alpha=1,nfolds = 10);
  lasso_model <- glmnet(x[train,],y[train],alpha = 1,lambda = lasso_cv_model$lambda.min,standardize=TRUE)
  lasso_fitt_value <- predict(lasso_model,newx = x[-train,])
  
  SubNbaPlayers_lasso <- SubNbaPlayers[c(array(unlist(predict(ridge_model,type="nonzero"))), pts_index)]
  
  lasso_test_mse_values[i] <- mean((y[-train] - lasso_fitt_value)^2)
  lasso_optimal_lambdas[i] <- lasso_cv_model$lambda.min
  lasso_optimal_cv_models[[i]] <- lasso_cv_model
  lasso_optimal_models[[i]] <- lasso_model
  lasso_optimal_lm[[i]] <- lm(pts ~ ., data = SubNbaPlayers_lasso[train,])
}

# Plot Lasso result
par(mfrow = c(1,1))
plot(lasso_optimal_cv_models[[1]])
summary(lasso_optimal_lm[[1]])
par(mfrow = c(2,2))
plot(lasso_optimal_lm[[1]])

par(mfrow = c(1,1))
plot(lasso_optimal_cv_models[[2]])
summary(lasso_optimal_lm[[2]])
par(mfrow = c(2,2))
plot(lasso_optimal_lm[[2]])

# Plot Ridge result
par(mfrow = c(1,1))
plot(ridge_optimal_cv_models[[1]])
summary(ridge_optimal_lm[[1]])
par(mfrow = c(2,2))
plot(ridge_optimal_lm[[1]])

par(mfrow = c(1,1))
plot(ridge_optimal_cv_models[[2]])
summary(ridge_optimal_lm[[2]])
par(mfrow = c(2,2))
plot(ridge_optimal_lm[[2]])

par(mfrow = c(1,1))


#### Final Model: removing fgm, fga

NbaPlayers <- subset(NbaPlayers, select = c(-fga,-fgm))

x <- model.matrix ( pts ~ . , NbaPlayers ) [ , -1]
y <- NbaPlayers$pts


### Ridge
ridge_cv_model <- cv.glmnet(x[train, ],y[train], lambda = lambda, alpha = 0, nfolds = 10)
plot(ridge_cv_model)
plot(glmnet(x[train,],y[train],alpha = 0,lambda = lambda,standardize=TRUE), xvar = "lambda")
ridge_cv_model$lambda.min

ridge_model <- glmnet(x[train,],y[train],alpha = 0,lambda = ridge_cv_model$lambda.min,standardize=TRUE)
ridge_fitt_value <- predict(ridge_model, newx = x[-train,])
ridge_test_mse <- mean((y[-train] - ridge_fitt_value)^2)
ridge_test_mse

### Lasso
lasso_cv_model <- cv.glmnet(x[train, ],y[train], lambda = lambda, alpha = 1, nfolds = 10)
plot(lasso_cv_model)
plot(glmnet(x[train,],y[train],alpha = 1,lambda = lambda,standardize=TRUE), xvar = "lambda")
lasso_cv_model$lambda.min

lasso_model <- glmnet(x[train,],y[train],alpha = 1,lambda = lasso_cv_model$lambda.min,standardize=TRUE)
lasso_fitt_value <- predict(lasso_model, newx = x[-train,])
lasso_test_mse <- mean((y[-train] - lasso_fitt_value)^2)
lasso_test_mse
