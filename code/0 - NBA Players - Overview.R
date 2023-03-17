
# NBA Players - Dataset Overview

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot


# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg.csv")

dim(NbaPlayers)

names(NbaPlayers)

head(NbaPlayers)

summary(NbaPlayers)

# Plots of various classes: Check the dataset balance
hist(NbaPlayers$TARGET_5Yrs,2)
plot(c(1:dim(NbaPlayers)[1]), NbaPlayers$TARGET_5Yrs)

PositiveOccurrences <- NbaPlayers[NbaPlayers$TARGET_5Yrs == 1,]
dim(PositiveOccurrences)

NegativeOccurrences <- NbaPlayers[NbaPlayers$TARGET_5Yrs == 0,]
dim(NegativeOccurrences)
