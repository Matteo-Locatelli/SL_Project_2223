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
hist(NbaPlayers$target_5yrs)

# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers), floor(nrow(NbaPlayers)*0.75))


### Subset Selection

log_reg_all <- glm( target ~ .-target_5yrs,
                    data = NbaPlayers , family = binomial)
summary(log_reg_all)

log_reg_null <- glm( target ~ 1,
                     data = NbaPlayers , family = binomial)
summary(log_reg_null)

## Forward stepwise

f_log_reg_fit <- regsubsets(x = NbaPlayers[, c(-18, -19)], 
                            y = NbaPlayers$target, 
                            data = NbaPlayers, method = "forward")

# Positive/Negative coefficient for a predictor
summary(f_log_reg_fit)

# plot residual
par(mfrow = c(1,2))

# plot 1
plot(f_log_reg_fit$residuals, pch = "o", col = "blue" , ylab = "Residual", 
     main = paste0("Residuals plot: mean=",round(mean(f_log_reg_fit$residuals),digits = 4),
                   " & var=", round(var(f_log_reg_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(f_log_reg_fit$residuals)), col= "red", lwd = 3)

# plot 2 
hist(f_log_reg_fit$residuals,40,
     xlab = "Residual",
     main = "Residuals empirical distribution") 

par(mfrow = c(1,1))


## Backward stepwise