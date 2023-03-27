
# Linear Regression
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


### Simple Linear Regression

#Fit Linear Model: pts = b0 + b1*min + e
lm_fit <- lm(pts ~ min, data = NbaPlayers)

# View fitted results
summary(lm_fit) # As expected if the minutes per game increase the points per game increase

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

### Check if there is a better regressor then min

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

plot(factor(all_regressors), r_squared_array)
plot(factor(all_regressors), mse)

#Fit Linear Model: pts = b0 + b1*fga + e
lm_fit <- lm(pts ~ fga, data = NbaPlayers)

# View fitted results
summary(lm_fit)

# Compute confident interval (CI) on coefficient
confint(lm_fit, level = 0.98)

# Plot linear regression results
plot(NbaPlayers$fga, NbaPlayers$pts, pch = "+", ylab = "pts - Points Scored Per Game", 
     xlab = "fga - Field Goal Attempt", 
     main = "Linear regression")
legend("topright",legend=c("Sampling point"),pch = "+")

# Add predicted value 
abline(lm_fit, lwd = 3, col = "red")

#  Create Subplot region  
par(mfrow = c(2, 2))
plot(lm_fit)

# plot residual
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



##### TO DELETE
### Validation method 

# Validation Method
set.seed(2)
train <- sample (dim(NbaPlayers)[1] , floor(dim(NbaPlayers)[1]*0.5), replace = FALSE)

lm_fit <- lm(pts ~ min , data = NbaPlayers , subset = train ) #we can provide a subset given by the indexes in 'train'
err = (NbaPlayers$pts - predict(lm_fit, NbaPlayers ))^2 # R understands that we want the difference with that output

trai_err = mean(err[train])
test_err = mean(err[-train])  # - tells to give all the indexes        

# quadratic fit
lm_fit_quad <- lm ( pts ~ poly ( min , 2) , data = NbaPlayers ,
                    subset = train )
test_quad = mean(( NbaPlayers$pts - predict (lm_fit_quad,NbaPlayers)) [-train ]^2)

# cubic fit
lm_fit_cubic <- lm( pts ~ poly ( min , 3) , data = NbaPlayers ,
                    subset = train )
test_cubic = mean(( NbaPlayers$pts - predict (lm_fit_cubic,NbaPlayers)) [-train ]^2)

# four fit
lm_fit_four <- lm( pts ~ poly ( min , 4) , data = NbaPlayers ,
                    subset = train )
test_four = mean(( NbaPlayers$pts - predict (lm_fit_four,NbaPlayers)) [-train ]^2)

par(mfrow = c(1, 1))
plot(1:4,c(test_err,test_quad,test_cubic,test_four), type = "b", col="blue",
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
  glm_fit <- glm( pts ~ poly ( min , i ) , data = NbaPlayers )
  loo_cv[ i ] <- cv.glm(NbaPlayers , glm_fit ,K = dim(NbaPlayers)[1])$delta[1]
} 

plot(1:10,loo_cv,type = "b",col = "blue",
     ylab = "CV error",
     xlab = "Flexibility (poly degree)",
     main = "Test error estimation")

# k-fold validation method (Biad variace - trade off k=5, k=10)
set.seed(1)
kfold <- rep (0 , 10)
for(i in 1:10) {
  glm_fit <- glm(pts ~ poly ( min , i ) , data = NbaPlayers )
  kfold[ i ] <- cv.glm( NbaPlayers , glm_fit , K = 10)$delta[1]
}

lines(1:10,kfold,type = "b",col = "red", lty = 2)
legend("topright",legend=c("LOOCV","k = 10"),col = c("blue","red"),lty = 1:2)


