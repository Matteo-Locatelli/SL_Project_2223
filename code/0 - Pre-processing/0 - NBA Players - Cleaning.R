
# NBA Players - Dataset cleaning

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot

library( janitor )

# set working directory
setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223")
#setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223")

NbaPlayers <- read.csv("./nba_logreg.csv")

dim(NbaPlayers)
names(NbaPlayers)
head(NbaPlayers)

## Data set cleaning

# removing duplicate data
NbaPlayersNoDup <- NbaPlayers[!duplicated(NbaPlayers), ]
numDuplicated <- dim(NbaPlayers)[1] - dim(NbaPlayersNoDup)[1]

# removing inconsistent data
NbaPlayersConsistent <- NbaPlayersNoDup[!duplicated(NbaPlayersNoDup[, -dim(NbaPlayersNoDup)[2]], fromLast = FALSE) & 
                                     !duplicated(NbaPlayersNoDup[, -dim(NbaPlayersNoDup)[2]], fromLast = TRUE), ]
numInconsistents <- dim(NbaPlayersNoDup)[1] - dim(NbaPlayersConsistent)[1]

# remove name column
NbaPlayersNoName <- NbaPlayersConsistent[, -1]

# remove na values
NbaPlayersNoNa <- na.omit(NbaPlayersNoName)
numNa <- dim(NbaPlayersNoName)[1] - dim(NbaPlayersNoNa)[1]

# clean columns names
NbaPlayersClean <- clean_names(NbaPlayersNoNa)

write.csv(NbaPlayersClean, "./nba_logreg_clean.csv", row.names=FALSE)
