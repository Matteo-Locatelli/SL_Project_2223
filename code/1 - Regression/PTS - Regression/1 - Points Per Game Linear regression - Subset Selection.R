
# Linear Regression: Subset Selection
# Target: Point Scored Per Game

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
library(caret)
# in console
# install.packages("devtools")
# devtools::install_github("rsquaredacademy/olsrr")
library(olsrr) # use dev tools
library(readr)
library(dplyr)
set.seed(1) # seed for random number generator

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)

NbaPlayers <- subset(NbaPlayers, select=-c(target_5yrs))


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

# Stepwise Backward Regression
b_step_reg <- ols_step_backward_p(b_lm_fit_all)
plot(b_step_reg)

# Stepwise Regression
step_reg <- ols_step_both_p(f_lm_fit)
plot(step_reg)
