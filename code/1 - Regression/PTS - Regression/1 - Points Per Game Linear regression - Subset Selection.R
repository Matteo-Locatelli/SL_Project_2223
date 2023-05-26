
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

# Remove rebounds and categorical variable
NbaPlayers <- subset(NbaPlayers, select = c(-reb))

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
hist(NbaPlayers$pts,
     xlab = "pts - Points per game",
     main = "pts histogram")


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
plot(f_lm_fit$residuals, pch = "o", col = "blue" , ylab = "Residual", 
     main = paste0("Residuals plot: mean=",round(mean(f_lm_fit$residuals),digits = 4),
     " & var=", round(var(f_lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(f_lm_fit$residuals)), col= "red", lwd = 3)

# plot 2 
hist(f_lm_fit$residuals,40,
     xlab = "Residual",
     main = "Residuals empirical distribution") 


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
plot(b_lm_fit$residuals, pch = "o", col = "blue" , ylab = "Residual", 
     main = paste0("Residuals plot: mean=",round(mean(b_lm_fit$residuals),digits = 4),
     " & var=", round(var(b_lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(b_lm_fit$residuals)), col= "red", lwd = 3)

# plot 2 
hist(b_lm_fit$residuals,40,
     xlab = "Residual",
     main = "Residuals empirical distribution") 


### Further analysis on the fitted models

# Stepwise Forward Regression
f_step_reg <- ols_step_forward_p(f_lm_fit_all)
summary(f_step_reg$model)
#plot(f_step_reg)

# Stepwise Backward Regression
b_step_reg <- ols_step_backward_p(b_lm_fit_all)
summary(b_step_reg$model)
#plot(b_step_reg)

# Forward contains dreb & ast that is not included in backward
# Backward contains oreb that are not included in forward

# Stepwise Regression
both_lm_fit_all <- lm(pts ~ ., data=NbaPlayers)
step_reg <- ols_step_both_p(both_lm_fit_all)
summary(step_reg$model)
#plot(step_reg)

# f_reg: -blk, -min, -dreb
# b_reg: +ast, -min, -oreb, -blk 


#### Subset selection with variables omitting

## Compute the R_squared for each variable

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

## Compute models with thresholds of correlation with pts

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

# Forward has one more variables: x3p_made. All the others are the same.
# The additional variable in the forward is not significant.

sprintf('### Threshold: %f', thresholds[2])
sprintf('## step forward model:')
summary(step_forward_models[[2]]$model)
sprintf('## step backward model:')
summary(step_backward_models[[2]]$model)

# Forward has one more variables: x3p_made. All the others are the same
# The additional variable in the forward is not significant

# The difference btw threshold 0.9 is that min is not there anymore and it has 
# been replaced by ft in both models


#### Check the model without the non significant variables
signif_model <- lm(pts ~ min + fg + x3pa + x3p + ftm + oreb + dreb + ast + stl + tov, data=NbaPlayers)
signif_model_t1 <- lm(pts ~ min + fg + x3pa + x3p + ftm + oreb + dreb + ast + stl + tov, data=NbaPlayers)
signif_model_t2 <- lm(pts ~ ft + fg + x3pa + x3p + ftm + oreb + dreb + ast + stl + tov + gp, data=NbaPlayers)


#### Final Model: leaving fgm, fga

library( boot )
library( boot.pval )

NbaPlayers <- subset(NbaPlayers, select = c(-fga,-fgm))

## Forward

# Model fitting

f_lm_all <- lm(pts ~ . -fta, data=NbaPlayers)
final_step_forward <- ols_step_forward_p(model = f_lm_all)
summary(final_step_forward)
final_step_forward_model <- lm(final_step_forward$model, data = NbaPlayers)
summary(final_step_forward_model)

# Estimate variance of coefficients with bootstrap

forward_fun_boot <- function(data,index){
  linear <- lm(final_step_forward_model, data = data, subset = index);
  return (linear$coefficients)
}

forward_boot <- boot(NbaPlayers, forward_fun_boot, R = 1000)
boot.pval(forward_boot, theta_null = rep(0, length(forward_boot$t0)))

boot_summary(final_step_forward_model, R = 1000)

# Remove not significant variables

forward_lm_fit <- lm(final_step_forward_model$model, data=NbaPlayers)
summary(forward_lm_fit)

forward_lm_fit <- update(forward_lm_fit, ~ . - x3p_made -gp - blk)
summary(forward_lm_fit)
sqrt(mean(forward_lm_fit$residuals^2))

# Plots

par(mfrow = c(1,2))

plot(forward_lm_fit$residuals, pch = "o", col = "blue" , ylab = "Residuals", 
     main = paste0("Residuals plot: mean=",round(mean(forward_lm_fit$residuals),digits = 4),
                   " & var=", round(var(forward_lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(forward_lm_fit$residuals)), col= "red", lwd = 3)

hist(forward_lm_fit$residuals,40,
     xlab = "Residual",
     main = "Residuals empirical distribution") 


## Backward

# Model fitting

b_lm_all <- lm(pts ~ ., data=NbaPlayers)
final_step_backward <- ols_step_backward_p(b_lm_all)
final_step_backward_model <- lm(final_step_backward$model, data = NbaPlayers)
summary(final_step_backward_model)

# Estimate variance of coefficients with bootstrap

backward_fun_boot <- function(data,index){
  linear <- lm(final_step_backward_model, data = data,subset = index);
  return (linear$coefficients)
}

backward_boot <- boot(NbaPlayers,backward_fun_boot,R = 1000)
boot.pval(backward_boot, theta_null = rep(0, length(backward_boot$t0)))

boot_summary(final_step_backward_model, R = 1000)

# Remove not significant variables

backward_lm_fit <- lm(final_step_backward_model$model, data=NbaPlayers)
summary(backward_lm_fit)

backward_lm_fit <- update(backward_lm_fit, ~ . -gp - blk)
summary(backward_lm_fit)

sqrt(mean(backward_lm_fit$residuals^2))

#backward_lm_fit <- update(backward_lm_fit, ~ . -oreb - stl)
#summary(backward_lm_fit)



# Plots

par(mfrow = c(1,2))

plot(backward_lm_fit$residuals, pch = "o", col = "blue" , ylab = "Residuals", 
     main = paste0("Residuals plot: mean=",round(mean(backward_lm_fit$residuals),digits = 4),
                   " & var=", round(var(backward_lm_fit$residuals),digits = 2)))

abline(c(0,0),c(0,length(backward_lm_fit$residuals)), col= "red", lwd = 3)

hist(backward_lm_fit$residuals,40,
     xlab = "Residual",
     main = "Residuals empirical distribution")
