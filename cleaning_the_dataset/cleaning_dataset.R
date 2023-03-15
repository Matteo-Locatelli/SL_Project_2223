
# NBA Players 
# - data-set overview
# - clean duplicates and inconsistencies

rm(list = ls()) # clear all environment variable
graphics.off()  # close all plot


# set working directory
# setwd("C:/Users/Wasim/Documents/Universita/Magistrale/Secondo Semestre/Statistical Learning/SL_Project_2223/dataset_choice/nba_players")
setwd("C:/Scuola/unibg/magistrale/II anno/II semestre/SL-Statistical_learning/SL_Project_2223/cleaning_the_dataset")

NbaPlayers <- read.csv("../nba_logreg.csv")

dim(NbaPlayers)

names(NbaPlayers)

head(NbaPlayers)

rows <- nrow(NbaPlayers)
cols <- ncol(NbaPlayers)

summary(NbaPlayers)


# Plots of various classes: check the data-set balance
hist(NbaPlayers$TARGET_5Yrs,2)
plot(c(1:dim(NbaPlayers)[1]), NbaPlayers$TARGET_5Yrs)

PositiveOccurrences <- NbaPlayers[NbaPlayers$TARGET_5Yrs == 1,]
dim(PositiveOccurrences)

NegativeOccurrences <- NbaPlayers[NbaPlayers$TARGET_5Yrs == 0,]
dim(NegativeOccurrences)


# get the indexes of duplicated values (duplicated() gets the first of the 2)

# NbaPlayers[1, ] == NbaPlayers[1, ]
# NbaPlayers[24, ]$Name == NbaPlayers[23, ]$Name && NbaPlayers[24, ]$TARGET_5Yrs == NbaPlayers[23, ]$TARGET_5Yrs

dup_array <- duplicated(NbaPlayers) # -> logical array

length(dup_array) # -> 1340

dup_array_length <- sum(dup_array == TRUE) # -> 12

dup_array_indeces <- c() # -> empty array

length(dup_array_indeces) # -> 0

dup_array_indeces # -> null

for (i in 1:rows){
  if(dup_array[i] == TRUE) {
    dup_array_indeces <- append(dup_array_indeces, i)
  }  
}

dup_array_indeces

length(dup_array_indeces) # -> 12
