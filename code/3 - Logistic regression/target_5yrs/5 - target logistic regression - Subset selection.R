# Logistic regression
# Target: target

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
library(ISLR2);
library(MASS);
library( caret )
# in console
# install.packages("devtools")
# devtools::install_github("rsquaredacademy/olsrr")
library( olsrr ) # use dev tools
library( readr )
library( dplyr )
library( leaps )
library( sp )
set.seed(1) # seed for random number generator

# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

## Pre-processing of data-set
NbaPlayers <- subset(NbaPlayers, select = c(-pts, -reb))
NbaPlayers$target <- factor( ifelse(NbaPlayers$target_5yrs == 0 , " No " , " Yes " ))

# View(NbaPlayers)
dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
#hist(NbaPlayers$target_5yrs)



### Subset Selection


## Forward stepwise

f_log_reg_fit <- regsubsets(x = NbaPlayers[, c(-18, -19)], 
                            y = NbaPlayers$target, 
                            data = NbaPlayers, method = "forward", 
                            nvmax = dim(NbaPlayers)[2]-2)

# Positive/Negative coefficient for a predictor
summary(f_log_reg_fit)

f_log_reg_sum <- summary(f_log_reg_fit)

plot( f_log_reg_sum$adjr2, pch = 18, lwd = 3, col = "red" ,
      ylab = "Adjusted R-squared", xlab = "Number of variables", 
      main = "Adjusted R squared of forward stepwise selection for logistic regression")
abline(v = which.max(f_log_reg_sum$adjr2), col="green", lwd = 3)

plot( f_log_reg_sum$rss, pch = 18, lwd = 3, col = "blue" ,
      ylab = "Residual Sum of Squares", xlab = "Number of variables", 
      main = "RSS of forward stepwise selection for logistic regression")
abline(v = which.min(f_log_reg_sum$rss), col="green", lwd = 3)


## Backward stepwise

b_log_reg_fit <- regsubsets(x = NbaPlayers[, c(-18, -19)], 
                            y = NbaPlayers$target, 
                            data = NbaPlayers, method = "backward", 
                            nvmax = dim(NbaPlayers)[2]-2)

# Positive/Negative coefficient for a predictor
summary(b_log_reg_fit)

b_log_reg_sum <- summary(b_log_reg_fit)

plot( b_log_reg_sum$adjr2, pch = 18, lwd = 3, col = "red" ,
      ylab = "Adjusted R-squared", xlab = "Number of variables", 
      main = "Adjusted R squared of backward stepwise selection for logistic regression")
abline(v = which.max(b_log_reg_sum$adjr2), col="green", lwd = 3)

plot( b_log_reg_sum$rss, pch = 18, lwd = 3, col = "blue" ,
      ylab = "Residual Sum of Squares", xlab = "Number of variables", 
      main = "RSS of backward stepwise selection for logistic regression")
abline(v = which.min(b_log_reg_sum$rss), col="green", lwd = 3)


## Exhaust stepwise

e_log_reg_fit <- regsubsets(x = NbaPlayers[, c(-18, -19)], 
                            y = NbaPlayers$target, 
                            data = NbaPlayers, method = "exhaust", 
                            nvmax = dim(NbaPlayers)[2]-2)

# Positive/Negative coefficient for a predictor
summary(e_log_reg_fit)

e_log_reg_sum <- summary(e_log_reg_fit)

plot( e_log_reg_sum$adjr2, pch = 18, lwd = 3, col = "red" ,
      ylab = "Adjusted R-squared", xlab = "Number of variables", 
      main = "Adjusted R squared of exhaust stepwise selection for logistic regression")
abline(v = which.max(e_log_reg_sum$adjr2), col="green", lwd = 3)

plot( e_log_reg_sum$rss, pch = 18, lwd = 3, col = "blue" ,
      ylab = "Residual Sum of Squares", xlab = "Number of variables", 
      main = "RSS of exhaust stepwise selection for logistic regression")
abline(v = which.min(e_log_reg_sum$rss), col="green", lwd = 3)



### Boosting
# Linear model (such as logistic regression) is not good for boosting. 
# The reason is if you add two linear models together, the result is another linear model. 
# On the other hand, adding two decision stumps or trees, will have a more complicated 
# and interesting model.
# https://stats.stackexchange.com/questions/329066/boosting-a-logistic-regression-model

library( boot )
library( boot.pval )

## Forward: f_log_reg_fit


## Backward: b_log_reg_fit