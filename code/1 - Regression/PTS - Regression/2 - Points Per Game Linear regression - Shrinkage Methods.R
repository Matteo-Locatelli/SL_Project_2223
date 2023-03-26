
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

## RIDGE REGRESSION

# Construct Design Matrices and also automatically transforms any qualitative 
# variables into dummy variables
x <- model.matrix ( pts ~ . , NbaPlayers ) [ , -1]
y <- NbaPlayers$pts

# lambda grid
lambda <- 10^seq(-2,1,length = 50);

# tuning elastics net (alpha 0 - RIDGE, alpha 1 - LASSO)
ridge_fit <- glmnet(x,y,alpha = 0,lambda = lambda,standardize=TRUE)
dim(coef(ridge_fit))

# compare l2-norm at different value of lambda
ridge_fit$lambda[5]
sqrt(sum(coef(ridge_fit)[-1,5]^2))

ridge_fit$lambda[25]
sqrt(sum(coef(ridge_fit)[-1,25]^2))

set.seed(1)
train <- sample(dim(x)[1],floor(dim(x)[1]*0.6),replace = FALSE);

# lambda = seq() parameters is optional 
cv_model <- cv.glmnet(x[train, ],y[train], alpha = 0, nfolds = 10);
plot(cv_model)
opt_lambda <- cv_model$lambda.min # cv_model$lambda.1se

# predict on test dataset
model <- glmnet(x[train,],y[train],alpha = 0,lambda = opt_lambda,standardize=TRUE)
fitt_value <- predict(model,newx = x[-train,])

test_MSE = mean((y[-train] - fitt_value)^2)


## LASSO
lasso_mod <- glmnet( x[train , ] , y[ train ], alpha = 1,lambda = lambda)
plot(lasso_mod)

set.seed(1)
cv_lasso <- cv.glmnet(x[train,],y[train],alpha=1,nfolds = 10);
plot(cv_lasso)
opt_lambda <- cv_lasso$lambda.min
