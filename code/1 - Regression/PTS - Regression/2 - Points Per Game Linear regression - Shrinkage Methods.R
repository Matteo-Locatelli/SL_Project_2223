
# Shrinkage Methods
# Target: Point Scored Per Game

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library ( boot )
library( glmnet )
set.seed (1)

# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)

### Shrinkage with full dataset

## RIDGE REGRESSION

# Construct Design Matrices and also automatically transforms any qualitative 
# variables into dummy variables
x <- model.matrix ( pts ~ . , NbaPlayers ) [ , -1]
y <- NbaPlayers$pts

# lambda grid
lambda <- 10^seq(-6,-2,length = 200);
lambda <- c(0,lambda)

set.seed(1)
train <- sample(dim(x)[1],floor(dim(x)[1]*0.6),replace = FALSE);

# lambda = seq() parameters is optional 
ridge_cv_model <- cv.glmnet(x[train, ],y[train], lambda = lambda, alpha = 0, nfolds = 10);
plot(ridge_cv_model)
opt_lambda <- ridge_cv_model$lambda.min; # cv_model$lambda.1se
opt_lambda
# predict on test dataset
ridge_model <- glmnet(x[train,],y[train],alpha = 0,lambda = opt_lambda,standardize=TRUE)
fitt_value <- predict(ridge_model,newx = x[-train,])

test_MSE = mean((y[-train] - fitt_value)^2)


## LASSO
lasso_mod <- glmnet( x[train , ] , y[ train ], alpha = 1,lambda = lambda)
plot(lasso_mod)

set.seed(1)
cv_lasso <- cv.glmnet(x[train,],y[train],lambda = lambda, alpha=1,nfolds = 10);
plot(cv_lasso)
opt_lambda <- cv_lasso$lambda.min
opt_lambda

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
for(i in 1:2){
  colSelection <- c(all_regressors[r_squared_array <= thresholds[i]], "pts")
  SubNbaPlayers <- NbaPlayers[colSelection]
  
  x <- model.matrix ( pts ~ . , SubNbaPlayers) [ , -1]
  y <- SubNbaPlayers$pts
  set.seed(1)
  train <- sample(dim(x)[1],floor(dim(x)[1]*0.6),replace = FALSE);
  
  # Ridge
  ridge_cv_model <- cv.glmnet(x[train, ],y[train], lambda = lambda, alpha = 0, nfolds = 10);
  ridge_optimal_lambdas[i] <- ridge_cv_model$lambda.min;
  
  lasso_cv_model <- cv.glmnet(x[train,],y[train],lambda = lambda, alpha=1,nfolds = 10);
  lasso_optimal_lambdas[i] <- lasso_cv_model$lambda.min
}
