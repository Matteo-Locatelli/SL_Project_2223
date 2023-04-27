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
library( sp )
set.seed(1) # seed for random number generator

# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

## Pre-processing of data-set
NbaPlayers <- subset(NbaPlayers, select = c(-pts, -reb))
NbaPlayers$target <- factor( ifelse(NbaPlayers$target_5yrs == 0 , " No " , " Yes " ))
#colnames(NbaPlayers)[18] = "y"
# View(NbaPlayers)
dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)
#hist(NbaPlayers$target_5yrs)

### Subset Selection

# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers), floor(nrow(NbaPlayers)*0.75))

log_reg_all <- glm( target ~ .-target_5yrs,
                    data = NbaPlayers , subset = train, family = binomial)

## Forward stepwise

f_log_reg_fit <- stepAIC(log_reg_all, direction = "forward", trace = FALSE)

summary(f_log_reg_fit)

f_fit <- predict(f_log_reg_fit, NbaPlayers[-train,], type = "response")
hist(f_fit, main = "Histogram of probabilities prediction", 
     xlab = "Probability predicted",
     #ylim = c(0,30),
     breaks = 20, xaxt='n')
axis(side=1, at=seq(0, 1, 0.05))
abline(v = 0.45, col='red', lwd = 3)
abline(v = 0.55, col='red', lwd = 3)

# Boolean vector of 1250 values
f_class <- rep ( " No ", dim(NbaPlayers)[1]-length(train))
f_class[f_fit > .5] = " Yes ";

# Show confusion matrix
f_table <- table(f_class, NbaPlayers$target[-train])
f_table

# Evaluate the error rate
f_err_rate = (f_table[1,2] + f_table[2,1]) / sum(f_table)
f_err_rate

# Percentage of correct predictions
mean(f_class == NbaPlayers$target[-train])


## Backward stepwise

b_log_reg_fit <- stepAIC(log_reg_all, direction = "backward", trace = FALSE)

summary(b_log_reg_fit)

b_fit <- predict(b_log_reg_fit, NbaPlayers[-train,], type = "response")
hist(b_fit, main = "Histogram of probabilities prediction", 
     xlab = "Probability predicted",
     #ylim = c(0,30),
     breaks = 20, xaxt='n')
axis(side=1, at=seq(0, 1, 0.05))
abline(v = 0.45, col='red', lwd = 3)
abline(v = 0.55, col='red', lwd = 3)

# Boolean vector of 1250 values
b_class <- rep ( " No ", dim(NbaPlayers)[1]-length(train))
b_class[f_fit > .5] = " Yes ";

# Show confusion matrix
b_table <- table(f_class, NbaPlayers$target[-train])
b_table

# Evaluate the error rate
b_err_rate = (f_table[1,2] + f_table[2,1]) / sum(f_table)
b_err_rate

# Percentage of correct predictions
mean(b_class == NbaPlayers$target[-train])


## Model with only signficant variables

log_reg_final <- glm( target ~ gp + x3p_made + x3pa + oreb + ast,
                    data = NbaPlayers , subset = train, family = binomial)

summary(log_reg_final)

log_reg_final <- update(log_reg_final, ~ . - ast)
summary(log_reg_final)

log_reg_final <- update(log_reg_final, ~ . - x3pa)
summary(log_reg_final)

log_reg_final <- update(log_reg_final, ~ . - x3p_made)
summary(log_reg_final)

final_fit <- predict(log_reg_final, NbaPlayers[-train,], type = "response")
hist(final_fit, main = "Histogram of probabilities prediction", 
     xlab = "Probability predicted",
     #ylim = c(0,30),
     breaks = 20, xaxt='n')
axis(side=1, at=seq(0, 1, 0.05))
abline(v = 0.45, col='red', lwd = 3)
abline(v = 0.55, col='red', lwd = 3)

# Boolean vector of 1250 values
fit_class <- rep ( " No ", dim(NbaPlayers)[1]-length(train))
fit_class[final_fit > .5] = " Yes ";

# Show confusion matrix
fit_table <- table(fit_class, NbaPlayers$target[-train])
fit_table

# Evaluate the error rate
err_rate = (fit_table[1,2] + fit_table[2,1]) / sum(fit_table)
err_rate

# Percentage of correct predictions
mean(fit_class == NbaPlayers$target[-train])

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