
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
     xlim = c(10, 85),
     ylim = c(0, 240),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of games played",
     main = "GP histogram")
axis(side = 1, at=seq(10, 85, by=5))
axis(side = 2, at=seq(0, 240, by=40))

hist(NbaPlayers$min,
     xlim = c(0, 45),
     ylim = c(0, 350),
     axes = FALSE,
     labels = TRUE,
     xlab = "Minutes played per game",
     main = "MIN histogram")
axis(side = 1, at=seq(0, 45, by=5))
axis(side = 2, at=seq(0, 350, by=50))

hist(NbaPlayers$pts,
     xlim = c(0, 30),
     ylim = c(0, 350),
     axes = FALSE,
     labels = TRUE,
     xlab = "Points scored per game",
     main = "PTS histogram")
axis(side = 1, at=seq(0, 30, by=5))
axis(side = 2, at=seq(0, 350, by=50))

hist(NbaPlayers$fgm,
     xlim = c(0, 11),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Field goals made per game",
     main = "FGM histogram")
axis(side = 1, at=seq(0, 11, by=1))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$fga,
     xlim = c(0, 20),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Field goals attempts per game",
     main = "FGA histogram")
axis(side = 1, at=seq(0, 20, by=2))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$fg,
     xlim = c(20, 75),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Field goals percent per game",
     main = "FG% histogram")
axis(side = 1, at=seq(20, 75, by=5))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$x3p_made,
     xlim = c(0, 2.4),
     ylim = c(0, 900),
     axes = FALSE,
     labels = TRUE,
     xlab = "Three point shots made per game",
     main = "3P MADE histogram")
axis(side = 1, at=seq(0, 2.4, by=0.2))
axis(side = 2, at=seq(0, 900, by=100))

hist(NbaPlayers$x3pa,
     xlim = c(0, 6.5),
     ylim = c(0, 800),
     axes = FALSE,
     labels = TRUE,
     xlab = "3 point shots attempts per game",
     main = "3PA histogram")
axis(side = 1, at=seq(0, 6.5, by=0.5))
axis(side = 2, at=seq(0, 800, by=200))

hist(NbaPlayers$x3p,
     xlim = c(0, 100),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "3 point shots percent per game",
     main = "3P% histogram")
axis(side = 1, at=seq(0, 100, by=10))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$ftm,
     xlim = c(0, 8),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Free throws made per game",
     main = "FTM histogram")
axis(side = 1, at=seq(0, 8, by=0.5))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$fta,
     xlim = c(0, 11),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Free throws attempts per game",
     main = "FTA histogram")
axis(side = 1, at=seq(0, 11, by=1))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$ft,
     xlim = c(0, 100),
     ylim = c(0, 550),
     axes = FALSE,
     labels = TRUE,
     xlab = "Free throws percent per game",
     main = "FT% histogram")
axis(side = 1, at=seq(0, 100, by=10))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$oreb,
     xlim = c(0, 5.5),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of offensive rebounds per game",
     main = "OREB histogram")
axis(side = 1, at=seq(0, 5.5, by=0.5))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$dreb,
     xlim = c(0, 10),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of defensive rebounds per game",
     main = "DREB histogram")
axis(side = 1, at=seq(0, 10, by=1))
axis(side = 2, at=seq(0, 500, by=100))

hist(NbaPlayers$reb,
     xlim = c(0, 14),
     ylim = c(0, 400),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of rebounds per game",
     main = "REB histogram")
axis(side = 1, at=seq(0, 14, by=1))
axis(side = 2, at=seq(0, 400, by=100))

hist(NbaPlayers$ast,
     xlim = c(0, 11),
     ylim = c(0, 700),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of assists per game",
     main = "AST histogram")
axis(side = 1, at=seq(0, 11, by=1))
axis(side = 2, at=seq(0, 700, by=100))

hist(NbaPlayers$stl,
     xlim = c(0, 2.6),
     ylim = c(0, 400),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of steals per game",
     main = "STL histogram")
axis(side = 1, at=seq(0, 2.6, by=0.2))
axis(side = 2, at=seq(0, 400, by=100))

hist(NbaPlayers$blk,
     xlim = c(0, 4),
     ylim = c(0, 1100),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of blocks per game",
     main = "BLK histogram")
axis(side = 1, at=seq(0, 4, by=0.5))
axis(side = 2, at=seq(0, 1000, by=100))

hist(NbaPlayers$tov,
     xlim = c(0, 4.5),
     ylim = c(0, 500),
     axes = FALSE,
     labels = TRUE,
     xlab = "Number of turnovers per game",
     main = "TOV histogram")
axis(side = 1, at=seq(0, 4.5, by=0.5))
axis(side = 2, at=seq(0, 500, by=100))
