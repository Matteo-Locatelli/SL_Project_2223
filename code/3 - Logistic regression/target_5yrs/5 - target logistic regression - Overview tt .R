# Logistic regression
# Target: target

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library(ISLR2);
library(MASS);
library( boot )
library( boot.pval )

set.seed (1)

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

# split indexes vector with 75% of the data
train <- sample(1:nrow(NbaPlayers), floor(nrow(NbaPlayers)*0.75))


### Logistic regression with all predictors

log_reg_all <- glm( target ~ .-target_5yrs,
                    data = NbaPlayers , subset = train, family = binomial)

# Positive/Negative coefficient for a predictor
summary(log_reg_all)

coef(log_reg_all)

# Function to predict the probability of a rookie
fit_all <- predict( log_reg_all, NbaPlayers[-train,], type = "response")
hist(fit_all, main = "Histogram of probabilities prediction", 
     xlab = "Probability predicted", 
     ylim = c(0,30),
     breaks = 20, xaxt='n')
axis(side=1, at=seq(0, 1, 0.05))
abline(v = 0.45, col='red', lwd = 3)
abline(v = 0.55, col='red', lwd = 3)

# Boolean vector of 1250 values
clas_all <- rep ( " No ", dim(NbaPlayers)[1]-length(train))
clas_all[fit_all > .5] = " Yes ";

# Show confusion matrix
table_all <- table(clas_all, NbaPlayers$target[-train])
table_all

# Evaluate the error rate
err_rate_all = (table_all[1,2] + table_all[2,1]) / sum(table_all)
err_rate_all

# Percentage of correct predictions
mean(clas_all == NbaPlayers$target[-train])


# Estimate variance of coefficients with bootstrap

fun_boot <- function(data,index){
  log_reg <- glm( target ~ .-target_5yrs,
                 data = NbaPlayers , subset = index, family = binomial)
  return (log_reg$coefficients)
}

all_boot <- boot(NbaPlayers[train,], fun_boot,R = 1000)
boot.pval(all_boot, theta_null = rep(0, length(all_boot$t0)))

boot_summary(log_reg_all, R = 1000)

### Logistic regression with only important predictors

log_reg_imp <- glm( target ~ gp + oreb,
                    data = NbaPlayers , subset = train, family = binomial)

# Positive/Negative coefficient for a predictor
summary(log_reg_imp)

coef(log_reg_imp)

# Function to predict the probability of a rookie
fit_imp <- predict( log_reg_imp, NbaPlayers[-train,], type = "response")
hist(fit_imp, main = "Histogram of probabilities prediction", 
     xlab = "Probability predicted",
     ylim = c(0,30),
     breaks = 20, xaxt='n')
axis(side=1, at=seq(0, 1, 0.05))
abline(v = 0.45, col='red', lwd = 3)
abline(v = 0.55, col='red', lwd = 3)

# Boolean vector of 1250 values
clas_imp <- rep ( " No ", dim(NbaPlayers)[1]-length(train))
clas_imp[fit_imp > .5] = " Yes ";

# Show confusion matrix
table_imp <- table(clas_imp, NbaPlayers$target[-train])
table_imp

# Evaluate the error rate
err_rate_imp = (table_imp[1,2] + table_imp[2,1]) / sum(table_imp)
err_rate_imp

# Percentage of correct predictions
mean(clas_imp == NbaPlayers$target[-train])

# Slightly worse than before


### Try to fin the optimal classification threshold (standard 0.5) ### USELESS

test_err_rate = double(9)
p = double(9)

for(ptry in 1:9){
  log_reg <- glm( target ~ gp + oreb,
                  data = NbaPlayers, subset = train, family = binomial )
  fit <- predict( log_reg , NbaPlayers[-train,], type = "response")
  clas <- rep ( " No " , dim(NbaPlayers)[1]-length(train))
  p[ptry] = ptry*0.1
  clas[fit > p[ptry]] = " Yes ";
  table <- table(clas, NbaPlayers$target[-train])
  test_err_rate[ptry] = (table[1,2] + table[2,1]) / sum(table)
}

plot(p, test_err_rate, type='b', main="Logistic Regression Test Error(p)",
     ylab="test err rate", xlab="p", col='blue', pch=18,
     lwd=3, xaxp = c(0, 0.9, 9))

which.min(test_err_rate) # 3
min(test_err_rate)

# Try 30 values from 0.2 to 0.5 for optimal threshold

test_err_rate2 = double(30)
p2 = double(30)

for(ptry in 1:30){
  log_reg2 <- glm( target ~ gp + oreb,
                   data = NbaPlayers , subset = train, family = binomial )
  fit2 <- predict( log_reg2, NbaPlayers[-train,], type = "response")
  clas2 <- rep ( " No " , dim(NbaPlayers)[1]-length(train))
  p2[ptry] = 0.2 + ptry*0.01
  clas2[fit2 > p2[ptry]] = " Yes ";
  table2 <- table(clas2, NbaPlayers$target[-train])
  test_err_rate2[ptry] = (table2[1,2] + table2[2,1]) / sum(table2)
}

plot(p2, test_err_rate2, type='b', main="Logistic Regression Test Error(p)",
     ylab="test err rate", xlab="p", col='blue', pch=18,
     lwd=3, xaxp = c(0.2, 0.5, 30))

which.min(test_err_rate2) # minimum for p = 0.36
min(test_err_rate2)


# Logistic regression with optimum threshold

log_reg_opt <- glm( target ~ gp + oreb,
                    data = NbaPlayers , subset = train, family = binomial)

# Positive/Negative coefficient for a predictor
summary(log_reg_opt)

coef(log_reg_opt)

# Function to predict the probability of a rookie
fit_opt <- predict( log_reg_opt, NbaPlayers[-train,], type = "response")

# Boolean vector of 1250 values
clas_opt <- rep ( " No ", dim(NbaPlayers)[1]-length(train))
clas_opt[fit_opt > .36] = " Yes ";

# Show confusion matrix
table_opt <- table(clas_opt, NbaPlayers$target[-train])
table_opt

# Evaluate the error rate
err_rate_opt = (table_opt[1,2] + table_opt[2,1]) / sum(table_opt)
err_rate_opt

# Percentage of correct predictions
mean(clas_opt == NbaPlayers$target[-train])
