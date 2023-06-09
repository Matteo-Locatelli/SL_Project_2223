
# NBA Players- Data-set Overview

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223/dataset_choice/nba_players")

NbaPlayers <- read.csv("./nba_logreg.csv")

dim(NbaPlayers)

head(NbaPlayers)

# Plots of various classes
hist(NbaPlayers$TARGET_5Yrs,2)
plot(c(1:dim(NbaPlayers)[1]), NbaPlayers$TARGET_5Yrs)

PositiveOccurrences <- NbaPlayers[NbaPlayers$TARGET_5Yrs == 1,]
dim(PositiveOccurrences)

NegativeOccurrences <- NbaPlayers[NbaPlayers$TARGET_5Yrs == 0,]
dim(NegativeOccurrences)
