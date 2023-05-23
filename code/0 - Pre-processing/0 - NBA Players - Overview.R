
# NBA Players - Dataset Overview

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

# Import libraries
library(corrplot)

# set working directory
#setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg_clean.csv")

dim(NbaPlayers)

names(NbaPlayers)

head(NbaPlayers)

summary(NbaPlayers)

NbaPlayers_cor <- cor(NbaPlayers)
corrplot(NbaPlayers_cor)


### Plots of various classes: Check the data-set balance
number_of_zero <- sum(NbaPlayers$target_5yrs == 0)
number_of_one <- sum(NbaPlayers$target_5yrs == 1)
my_bar <- barplot(table(NbaPlayers$target_5yrs),
        beside=TRUE, 
        col=c(rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6)) ,
        ylim=c(0, 1000),
        main = "TARGET_5Yrs bar plot")

# Add the text 
text(my_bar, c(number_of_zero, number_of_one) + 30, paste(c(number_of_zero, number_of_one), sep="") ,cex=1) 

hist(NbaPlayers$target_5yrs, 
     xlim = c(0, 1),
     ylim = c(0, 1400),
     breaks = seq(0, 1, by = 0.5),
     labels = TRUE,
     xlab = NULL,
     ylab = NULL,
     main = "TARGET_5Yrs histogram")

plot(c(1:dim(NbaPlayers)[1]), NbaPlayers$target_5yrs)

PositiveOccurrences <- NbaPlayers[NbaPlayers$target_5yrs == 1,]
dim(PositiveOccurrences)

NegativeOccurrences <- NbaPlayers[NbaPlayers$target_5yrs == 0,]
dim(NegativeOccurrences)


### Plot the histograms of various regressors

hist(NbaPlayers$gp,
     ylim = c(0, 220),
     labels = TRUE,
     xlab = "Number of games played",
     main = "GP histogram")

hist(NbaPlayers$min,
     ylim = c(0, 350),
     labels = TRUE,
     xlab = "Minutes played per game",
     main = "MIN histogram")

hist(NbaPlayers$pts,
     ylim = c(0, 350),
     labels = TRUE,
     xlab = "Points scored per game",
     main = "PTS histogram")

hist(NbaPlayers$fgm,
     ylim = c(0, 500),
     labels = TRUE,
     xlab = "Field goals made per game",
     main = "FGM histogram")

hist(NbaPlayers$fga,
     ylim = c(0, 450),
     labels = TRUE,
     xlab = "Field goals attempts per game",
     main = "FGA histogram")

hist(NbaPlayers$fg,
     ylim = c(0, 450),
     labels = TRUE,
     xlab = "Field goals percent per game",
     main = "FG% histogram")

hist(NbaPlayers$x3p_made,
     xlim = c(0, 2.5),
     ylim = c(0, 900),
     labels = TRUE,
     xlab = "Three point shots made per game",
     main = "3P MADE histogram")

hist(NbaPlayers$x3pa,
     xlim = c(0, 7),
     ylim = c(0, 800),
     labels = TRUE,
     xlab = "3 point shots attempts per game",
     main = "3PA histogram")

hist(NbaPlayers$x3p,
     ylim = c(0, 500),
     labels = TRUE,
     xlab = "3 point shots percent per game",
     main = "3P% histogram")

hist(NbaPlayers$ftm,
     ylim = c(0, 450),
     labels = TRUE,
     xlab = "Free throws made per game",
     main = "FTM histogram")

hist(NbaPlayers$fta,
     xlim = c(0, 11),
     ylim = c(0, 500),
     labels = TRUE,
     xlab = "Free throws attempts per game",
     main = "FTA histogram")

hist(NbaPlayers$ft,
     ylim = c(0, 550),
     labels = TRUE,
     xlab = "Free throws percent per game",
     main = "FT% histogram")

hist(NbaPlayers$oreb,
     ylim = c(0, 450),
     labels = TRUE,
     xlab = "Number of offensive rebounds per game",
     main = "OREB histogram")

hist(NbaPlayers$dreb,
     xlim = c(0, 10),
     ylim = c(0, 500),
     labels = TRUE,
     xlab = "Number of defensive rebounds per game",
     main = "DREB histogram")

hist(NbaPlayers$reb,
     ylim = c(0, 400),
     labels = TRUE,
     xlab = "Number of rebounds per game",
     main = "REB histogram")

hist(NbaPlayers$ast,
     xlim = c(0, 11),
     ylim = c(0, 650),
     labels = TRUE,
     xlab = "Number of assists per game",
     main = "AST histogram")

hist(NbaPlayers$stl,
     xlim = c(0, 2.5),
     ylim = c(0, 400),
     labels = TRUE,
     xlab = "Number of steals per game",
     main = "STL histogram")

hist(NbaPlayers$blk,
     ylim = c(0, 1100),
     labels = TRUE,
     xlab = "Number of blocks per game",
     main = "BLK histogram")

hist(NbaPlayers$tov,
     ylim = c(0, 500),
     labels = TRUE,
     xlab = "Number of turnovers per game",
     main = "TOV histogram")
