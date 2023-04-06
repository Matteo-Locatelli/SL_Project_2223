
# NBA Players - Dataset Overview

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot


# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)

names(NbaPlayers)

head(NbaPlayers)

summary(NbaPlayers)

# Plots of various classes: Check the data-set balance
barplot(table(NbaPlayers$target_5yrs),
        beside=TRUE, 
        ylim=range(pretty(c(0, dim(NbaPlayers)[1]))),
        ylab = "Frequency",
        main = "Bar Plot of target_5yrs")

hist(NbaPlayers$target_5yrs, 
     xlim = c(0, 1),
     ylim = c(0, 1000),
     breaks = seq(0, 1, by = 0.5),
     labels = TRUE,
     xlab = levels(NbaPlayers$target_5yrs),
     ylab = "Frequency",
     main = "Histogram of target_5yrs")

plot(c(1:dim(NbaPlayers)[1]), NbaPlayers$target_5yrs)

PositiveOccurrences <- NbaPlayers[NbaPlayers$target_5yrs == 1,]
dim(PositiveOccurrences)

NegativeOccurrences <- NbaPlayers[NbaPlayers$target_5yrs == 0,]
dim(NegativeOccurrences)
