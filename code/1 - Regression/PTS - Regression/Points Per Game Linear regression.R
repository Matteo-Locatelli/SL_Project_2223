
rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
library(MASS)
library(ISLR2)
set.seed(1) # seed for random number generator

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)

x <- subset(NbaPlayers, select = -c(pts))
y <- NbaPlayers$pts


## Simple Linear Regression

### Fit Linear Model: mdev = b0 + b1*lstat + e
lm_fit <- lm(pts ~ min, data = NbaPlayers)

# View fitted results
lm_fit # As expected if the minutes per game increase the points per game increase
summary(lm_fit)
lm_fit$coefficients
# names(lm.fit)
# coef(lm.fit)

# Compute confident interval (CI) on coefficient
confint(lm_fit, level = 0.98)

# Get prediction with CI over new observation 
predict(lm_fit, data.frame(min = (c(10, 15, 20, 25, 30))),
        interval = "confidence")

# Plot linear regression results
plot(NbaPlayers$min, NbaPlayers$pts, pch = "+", ylab = "pts - points scored per game", 
       xlab = "min - minutes played per game", 
       main = "Linear regression")
legend("topright",legend=c("Sampling point"),pch = "+")

# Add predicted value 
abline(lm_fit, lwd = 3, col = "red")

#  Create Subplot region  
par(mfrow = c(2, 2))
plot(lm_fit)
# A strong pattern in the residuals indicates non-linearity in the data
# plot(predict(lm.fit), residuals(lm.fit))
# plot(predict(lm.fit), rstudent(lm.fit))

# If the residual plot indicates that there are non-linear associations in the
# data, then a simple approach is to use non-linear transformations of the
# predictors; one possible solution is to transform the response;

# Leverage misure

# Put plotting arrangement back to its original state
par(mfrow = c(1, 1))
plot(hatvalues(lm_fit), col="blue", 
     ylab = "leverage",
     xlab = "Observation",
     main = "leverage") # 1/n < leverage < 1 


## Multiple Linear Regression

# y =b0 + b1*lstat + b2*age + e
lm_fit <- lm(medv ~ lstat + age, data = Boston) # For some reason higher age -> higher price.
summary(lm_fit) # R-squared is almost the same, means age is not helpful

# y = b*X + e (perform a regression using all of the predictors)
lm_fit <- lm(medv ~ ., data = Boston)
s <- summary(lm_fit) 
R2 = s$adj.r.squared
RSE = s$sigma

# update regression, remove age predictor (large p-value)
lm_fit <- update(lm_fit, ~ . - age) # remove age and fit again the model

## Interaction Terms & Non-linear Transformations of the Predictors
summary(lm(medv ~ lstat + age + lstat:age, data = Boston))
summary(lm(medv ~ lstat + I(lstat^2), data = Boston))

# plot residual
lm_fit <- lm(medv ~ lstat + I(lstat^2) + log(rm), data = Boston); 
par(mfrow = c(1,2))

# plot 1
plot(lm_fit$residuals, pch = "o", col = "blue" ,
     ylab = "Residual", main = paste0("Residual plot - mean:",round(mean(lm_fit$residuals),digits = 4),
                                      "- var:", round(var(lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 2)

# plot 2 
hist(lm_fit$residuals,40,
     xlab = "Residual",
     main = "Distribuzione empirica dei residui") 


## Qualitative Predictors
head(Carseats)
View(Carseats)

# Given a qualitative variable such as Shelveloc , R generates dummy variables
# automatically in this way: 
contrasts(Carseats$ShelveLoc) # In this case it could create more varaibles (n-1) to map the quali. var. in quant.

# Regression
lm_fit <- lm(Sales ~ . + Income:Advertising + Price:Age, 
             data = Carseats)
summary(lm_fit)

### Validation method 

View(Auto)
dim(Auto)

# Validation Method
?sample
set.seed(2)
train <- sample (392 , 196, replace = FALSE)

lm_fit <- lm(mpg ~ horsepower , data = Auto , subset = train ) #we can provide a subset given by the indexes in 'train'
err = (Auto$mpg - predict(lm_fit, Auto ))^2 # R understands that we want the difference with that output

trai_err = mean(err[train])
test_err = mean(err[-train])  # - tells to give all the indexes        

# quadratic fit
lm_fit_quad <- lm ( mpg ~ poly ( horsepower , 2) , data = Auto ,
                    subset = train )
test_quad = mean(( Auto$mpg - predict (lm_fit_quad,Auto)) [-train ]^2)

# cubic fit
lm_fit_cubic <- lm( mpg ~ poly ( horsepower, 3) , data = Auto ,
                    subset = train )
test_cubic = mean(( Auto$mpg - predict (lm_fit_cubic,Auto)) [-train ]^2)

par(mfrow = c(1, 1))
plot(1:3,c(test_err,test_quad,test_cubic), type = "b", col="blue",
     ylab = "Test MSE",
     xlab = "Flexibility",
     main = "Validation Test MSE")


# Leave-one-out cross-validation (LOOCV) method
library(boot)
#?glm()
#?cv.glm() :: delta (1x2) vector contain the cross-validation results, The
# first element is the standard k-fold CV estimate. The second is a biascorrected 
# version

set.seed(1)
loo_cv <- rep (0 , 10)
for(i in 1:10){
  glm_fit <- glm( mpg ~ poly ( horsepower , i ) , data = Auto )
  loo_cv[ i ] <- cv.glm(Auto , glm_fit ,K = dim(Auto)[1])$delta[1]
} 

plot(1:10,loo_cv,type = "b",col = "blue",
     ylab = "CV error",
     xlab = "Flexibility (poly degree)",
     main = "Test error estimation")

# k-fold validation method (Biad variace - trade off k=5, k=10)
set.seed(1)
kfold <- rep (0 , 10)
for(i in 1:10) {
  glm_fit <- glm(mpg ~ poly ( horsepower , i ) , data = Auto )
  kfold[ i ] <- cv.glm( Auto , glm_fit , K = 10)$delta[1]
}

lines(1:10,kfold,type = "b",col = "red", lty = 2)
legend("topright",legend=c("LOOCV","k = 10"),col = c("blue","red"),lty = 1:2)


