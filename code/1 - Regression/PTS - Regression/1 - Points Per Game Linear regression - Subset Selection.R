
# Linear Regression: Subset Selection
# Target: Point Scored Per Game

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
library( caret )
# in console
# install.packages("devtools")
# devtools::install_github("rsquaredacademy/olsrr")
library( olsrr ) # use dev tools
library( readr )
library( dplyr )
set.seed(1) # seed for random number generator

# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

NbaPlayers <- NbaPlayers[-15]

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)

### Subset Selection

## Forward stepwise

#define model with 0 predictors -> mean(pts)
f_lm_fit_null <- lm(pts ~ 1, data=NbaPlayers)

#define model with all predictors
f_lm_fit_all <- lm(pts ~ ., data=NbaPlayers)

#perform forward stepwise regression
f_lm_fit <- step(f_lm_fit_null, direction='forward', scope=formula(f_lm_fit_all), trace=0)

#view final model
summary(f_lm_fit)

# plot residual
par(mfrow = c(1,2))

# plot 1
plot(f_lm_fit$residuals, pch = "o", col = "blue" ,
     ylab = "Residual", main = paste0("Residual plot - mean:",round(mean(f_lm_fit$residuals),digits = 4),
                                      "- var:", round(var(f_lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(f_lm_fit$residuals)), col= "red", lwd = 2)

# plot 2 
hist(f_lm_fit$residuals,40,
     xlab = "Residual",
     main = "Distribuzione empirica dei residui") 


## Backward stepwise

#define model with 0 predictors -> mena(pts)
b_lm_fit_null <- lm(pts ~ 1, data=NbaPlayers)

#define model with all predictors
b_lm_fit_all <- lm(pts ~ ., data=NbaPlayers)

#perform backward stepwise regression
b_lm_fit <- step(b_lm_fit_all, direction='backward', scope=formula(b_lm_fit_null), trace=0)

#view final model
summary(b_lm_fit)

# plot residual
par(mfrow = c(1,2))

# plot 1
plot(b_lm_fit$residuals, pch = "o", col = "blue" ,
     ylab = "Residual", main = paste0("Residual plot - mean:",round(mean(b_lm_fit$residuals),digits = 4),
                                      "- var:", round(var(b_lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(b_lm_fit$residuals)), col= "red", lwd = 2)

# plot 2 
hist(b_lm_fit$residuals,40,
     xlab = "Residual",
     main = "Distribuzione empirica dei residui") 


## Models comparison

# defining training control as Leave One Out Cross Validation
train_control <- trainControl(method = "LOOCV")

f_lm_fit_cv <- train(pts ~ fgm + ftm + x3p_made + fga + fg + fta + ft + 
                 x3pa + ast + x3p, data = NbaPlayers,
               method = "lm",
               trControl = train_control)

b_lm_fit_cv <- train(pts ~ min + fgm + fga + fg + x3p_made + x3pa + x3p + 
                       ftm + fta + ft + reb, data = NbaPlayers,
                     method = "lm",
                     trControl = train_control)


## Further analysis on the fitted models

# Stepwise Forward Regression
f_step_reg <- ols_step_forward_p(f_lm_fit_all)
plot(f_step_reg)
summary(f_step_reg$model)

# Stepwise Backward Regression
b_step_reg <- ols_step_backward_p(b_lm_fit_all)
plot(b_step_reg)
summary(b_step_reg$model)

# Backward contains oreb and dreb that are not inlcuded in forward
# Forward contains tov  that is not included in backward

# Stepwise Regression
both_lm_fit_all <- lm(pts ~ ., data=NbaPlayers)
step_reg <- ols_step_both_p(both_lm_fit_all)
plot(step_reg)


#### Subset selection with variables omitting


### Compute the R_squared for each variable

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

thresholds = c(0.9,0.8) # We stop at 0.8 otherwise we would ho down to 0.5 as R^2
step_forward_models <- vector(mode = "list", length = 2)
step_backward_models <- vector(mode = "list", length = 2)
for(i in 1:2){
  colSelection <- c(all_regressors[r_squared_array <= thresholds[i]], "pts")
  SubNbaPlayers <- NbaPlayers[colSelection]
  f_lm_all <- lm(pts ~ ., data=SubNbaPlayers)
  step_forward_models[[i]] <- ols_step_forward_p(f_lm_all)
  b_lm_all <- lm(pts ~ ., data=SubNbaPlayers)
  step_backward_models[[i]] <- ols_step_backward_p(b_lm_all)
}


sprintf('### Threshold: %f', thresholds[1])
sprintf('## step forward model:')
summary(step_forward_models[[1]]$model)
sprintf('## step backward model:')
summary(step_backward_models[[1]]$model)

# Forward has two more variables: fta - x3p_made. All the others are the same
# the additional variables in the forward are not significant

sprintf('### Threshold: %f', thresholds[2])
sprintf('## step forward model:')
summary(step_forward_models[[2]]$model)
sprintf('## step backward model:')
summary(step_backward_models[[2]]$model)

# Forward has two more variables: fta - x3p_made. All the others are the same
# the additional variables in the forward are not significant
# The difference btw threshold 0.9 is that min is not there anymore and it's replaced by ft

#### Check the model without the non significan vars
signif_model <- lm(pts ~ min + fg + x3pa + x3p + ftm + oreb + dreb + ast + stl + tov, data=NbaPlayers)
signif_model_t1 <- lm(pts ~ min + fg + x3pa + x3p + ftm + oreb + dreb + ast + stl + tov, data=NbaPlayers)
signif_model_t2 <- lm(pts ~ ft + fg + x3pa + x3p + ftm + oreb + dreb + ast + stl + tov + gp, data=NbaPlayers)


#### Final Model: leaving fgm, fga

library( boot )
library( boot.pval )

NbaPlayers <- subset(NbaPlayers, select = c(-fga,-fgm))

### Forward

## Model fitting
f_lm_all <- lm(pts ~ ., data=NbaPlayers)
final_step_forward <- ols_step_forward_p(f_lm_all)
final_step_forward_model <- lm(final_step_forward$model, data = NbaPlayers)
summary(final_step_forward_model)

## Estimate variance of coeff. with bootstrap

forward_fun_boot <- function(data,index){
  linear <- lm(final_step_forward_model, data = data,subset = index);
  return (linear$coefficients)
}

forward_boot <- boot(NbaPlayers,forward_fun_boot,R = 1000)
boot.pval(forward_boot, theta_null = rep(0, length(forward_boot$t0)))

boot_summary(final_step_forward_model, R = 1000)

#forward_lm_fit <- lm(final_step_forward_models$model, data=NbaPlayers)
#forward_lm_fit <- update(forward_lm_fit, ~ . - fta - x3p_made - gp - blk)
#summary(forward_lm_fit)


### Backward

## Model fitting
b_lm_all <- lm(pts ~ ., data=NbaPlayers)
final_step_backward <- ols_step_backward_p(b_lm_all)
final_step_backward_model <- lm(final_step_backward$model, data = NbaPlayers)
summary(final_step_backward_model)

## Estimate variance of coeff. with bootstrap

backward_fun_boot <- function(data,index){
  linear <- lm(final_step_backward_model, data = data,subset = index);
  return (linear$coefficients)
}

backward_boot <- boot(NbaPlayers,backward_fun_boot,R = 1000)
boot.pval(backward_boot, theta_null = rep(0, length(backward_boot$t0)))

boot_summary(final_step_backward_model, R = 1000)