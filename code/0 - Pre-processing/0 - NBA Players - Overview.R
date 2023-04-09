
# NBA Players - Dataset Overview

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot


# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)

names(NbaPlayers)

head(NbaPlayers)

summary(NbaPlayers)

# Plots of various classes: Check the data-set balance
number_of_zero <- sum(NbaPlayers$target_5yrs == 0)
number_of_one <- sum(NbaPlayers$target_5yrs == 1)
my_bar <- barplot(table(NbaPlayers$target_5yrs),
        beside=TRUE, 
        col=c(rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6)) ,
        ylim=c(0, 1000),
        main = "Target 5 Years bar plot")

# Add the text 
text(my_bar, c(number_of_zero, number_of_one) + 30, paste(c(number_of_zero, number_of_one), sep="") ,cex=1) 

hist(NbaPlayers$target_5yrs, 
     xlim = c(0, 1),
     ylim = c(0, 1400),
     breaks = seq(0, 1, by = 0.5),
     labels = TRUE,
     xlab = NULL,
     ylab = NULL,
     main = "target_5yrs histogram")

plot(c(1:dim(NbaPlayers)[1]), NbaPlayers$target_5yrs)

PositiveOccurrences <- NbaPlayers[NbaPlayers$target_5yrs == 1,]
dim(PositiveOccurrences)

NegativeOccurrences <- NbaPlayers[NbaPlayers$target_5yrs == 0,]
dim(NegativeOccurrences)
