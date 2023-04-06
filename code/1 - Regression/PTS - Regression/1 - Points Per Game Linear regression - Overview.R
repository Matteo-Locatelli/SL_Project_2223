
# Linear Regression
# Target: Point Scored Per Game

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

### Add Libraries
library(corrplot)
set.seed(1) # seed for random number generator

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

# Pre-processing of the dataset
NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)

NbaPlayers.cor <- cor(NbaPlayers)
corrplot(NbaPlayers.cor)


### Simple Linear Regression

## Fit Linear Model: pts = b0 + b1*min + e
lm_fit <- lm(pts ~ min, data = NbaPlayers)

# View fitted results
summary(lm_fit) # As expected if the minutes per game increase the points per game increase

# Compute confident interval (CI) on coefficient
confint(lm_fit, level = 0.98)

# Get prediction with CI over new observation 
predict(lm_fit, data.frame(min = (c(10, 15, 20, 25, 30))),
        interval = "confidence")

# Plot linear regression results
plot(NbaPlayers$min, NbaPlayers$pts, pch = "+", 
     ylab = "pts - points scored per game", 
     xlab = "min - minutes played per game", 
     main = "Linear regression")
legend("topright", legend=c("Sampling point"), pch = "+")

# Add predicted value 
abline(lm_fit, lwd = 3, col = "red")

#  Create Subplot region  
par(mfrow = c(2, 2))
plot(lm_fit)
# A strong pattern in the residuals indicates non-linearity in the data
par(mfrow = c (1,1))
plot (residuals(lm_fit), pch = "o", col = "blue" , ylab = "Residuals", 
      main = paste0("Residuals plot: mean=", round(mean(lm_fit$residuals), digits = 4),
                    " & var=", round(var(lm_fit$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 3)

# plot(predict(lm.fit), residuals(lm.fit))
# plot(predict(lm.fit), rstudent(lm.fit))

hist(lm_fit$residuals, 40,
     xlab = "Residuals",
     main = "Residuals empirical distribution") 


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

par(mfrow = c (1,1))
plot(factor(all_regressors), r_squared_array, 
     ylab = "R squared", 
     xlab = "Regressor", 
     main = "R squared of linear regression with pts")
plot(factor(all_regressors), mse, 
     ylab = "Mse", 
     xlab = "Regressor", 
     main = "Mse of linear regression with pts")


### Fit Linear Model with best regressor: pts = b0 + b1*fga + e
lm_fit <- lm(pts ~ fga, data = NbaPlayers)

# View fitted results
summary(lm_fit)

# Compute confident interval (CI) on coefficient
confint(lm_fit, level = 0.98)

# Plot linear regression results
plot(NbaPlayers$fga, NbaPlayers$pts, pch = "+", 
     ylab = "pts - Points Scored Per Game", 
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

plot(lm_fit$residuals, pch = "o", col = "blue" , ylab = "Residuals", 
     main = paste0("Residuals plot: mean=", round(mean(lm_fit$residuals), digits = 4),
                    " & var=", round(var(lm_fit$residuals),digits = 2)))

abline(c(0,0),c(0,length(lm_fit$residuals)), col= "red", lwd = 2)

hist(lm_fit$residuals, 40,
     xlab = "Residual",
     main = "Residuals empirical distribution") 


### Model fitted with the most important variables for pts

lm_fit_imp <- lm(pts ~ fgm + x3p_made + ftm, data=NbaPlayers)
summary(lm_fit_imp) # There is a linear combination: pts = 2*fgm + x3p_made + ftm

max(lm_fit_imp$residuals)

# Compute confident interval (CI) on coefficient
confint(lm_fit_imp, level = 0.98)

#  Create Subplot region  
par(mfrow = c(2, 2))
plot(lm_fit_imp)

# plot residual
par(mfrow = c(1,2))

# plot 1
plot(lm_fit_imp$residuals, pch = "o", col = "blue" , ylab = "Residual", 
     main = paste0("Residuals plot: mean= ", round(mean(lm_fit_imp$residuals), digits = 4),
                   " & var=", round(var(lm_fit_imp$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit_imp$residuals)), col= "red", lwd = 3)

# plot 2 
hist(lm_fit_imp$residuals, 40,
     xlab = "Residual",
     main = "Residuals empirical distribution") 


### Evaluate linear dependency on pts

y_hat_pts <- 2*(NbaPlayers['fgm'] - NbaPlayers['x3p_made']) + 
             3*NbaPlayers['x3p_made'] + NbaPlayers['ftm']
summary(lm(fga ~ fgm - 1, data=NbaPlayers))

max(NbaPlayers['pts'] - y_hat_pts)


### Model fitted to explain linear dependency on rebounds

lm_fit_reb <- lm(reb ~ oreb + dreb, data=NbaPlayers)
summary(lm_fit_reb) # There is a linear combination: reb = oreb + dreb

max(lm_fit_reb$residuals)

# Compute confident interval (CI) on coefficient
confint(lm_fit_reb, level = 0.98)

#  Create Subplot region  
par(mfrow = c(2, 2))
plot(lm_fit_reb)

# plot residual
par(mfrow = c(1,2))

# plot 1
plot(lm_fit_reb$residuals, pch = "o", col = "blue" , ylab = "Residuals", 
     main = paste0("Residuals plot: mean=", round(mean(lm_fit_reb$residuals), digits = 4),
                   " & var=", round(var(lm_fit_reb$residuals),digits = 2)))
abline(c(0,0),c(0,length(lm_fit_reb$residuals)), col= "red", lwd = 3)

# plot 2 
hist(lm_fit_reb$residuals, 40,
     xlab = "Residual",
     main = "Residuals empirical distribution") 


### Evaluate linear dependency on reb
y_hat_reb <- NbaPlayers['oreb'] + NbaPlayers['dreb']

max(NbaPlayers['reb'] - y_hat_reb)
