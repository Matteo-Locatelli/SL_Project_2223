
# Linear Regression: Subset Selection
# Target: Point Scored Per Game

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
set.seed(1) # seed for random number generator

# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)

### Subset Selection

## Forward stepwise

f_lm_fit_null <- lm(pts ~ 1, data=NbaPlayers)

#define model with all predictors
f_lm_fit_all <- lm(pts ~ ., data=NbaPlayers)

#perform forward stepwise regression
f_lm_fit <- step(f_lm_fit_null, direction='forward', scope=formula(f_lm_fit_all), trace=0)

#view final model
summary(f_lm_fit)

## Backward stepwise
b_lm_fit_null <- lm(pts ~ 1, data=NbaPlayers)

#define model with all predictors
b_lm_fit_all <- lm(pts ~ ., data=NbaPlayers)

#perform forward stepwise regression
b_lm_fit <- step(b_lm_fit_all, direction='backward', scope=formula(b_lm_fit_all), trace=0)

#view final model
summary(b_lm_fit)
