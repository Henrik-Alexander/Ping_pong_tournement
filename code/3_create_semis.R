### Finals  ###########################
# Purpose: Create the pairings        #
# Author: Henrik-Alexander Schubert   #
# Date: 14.06.2023                    #
# E-Mail: maxpong@demogr.mpg.de       #
# Requirements:                       #                        
#######################################


### Setting --------------------------

# Install the packages
library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(readxl)

# Set a seed
set.seed(111)

### Create funciton "not in" ---------

`%!in%` <- Negate(`%in%`)

### Load the data --------------------


# Set the paths
d1 <- read_xlsx("raw/schedule_league1.xlsx")
d2 <- read_xlsx("raw/schedule_league2.xlsx")

# How many times have one both games?
winner1 <- d1$Winner[duplicated(d1$Winner)]
winner2 <- d2$winner[duplicated(d2$winner)]
winner1 <- winner1[winner1 != "Win"]
winner2 <- winner2[winner2 != "Win"]


### Select for semi-finals ----------

# Teams semis
n_teams <- 4

# How many teams to select
miss1 <- n_teams - length(winner1)
miss2 <- n_teams - length(winner2)

# Get the teams that have one only one game
cand1 <- d1$Winner[d1$Winner %!in% c(winner1, "Win")]
cand2 <- d2$winner[d2$winner %!in% c(winner2, "Win")]

# Randomly select the fill teams ------
fill1 <- sample(cand1, miss1, replace = FALSE)
fill2 <- sample(cand2, miss2, replace = FALSE)

### Create the semi-finals --------

# Combine the data
finalists1 <- c(fill1, winner1)
finalists2 <- c(fill2, winner2)

# Create the matches 
match1.1 <- sample(finalists1, size = 2, replace = FALSE)
match1.2 <- finalists1[finalists1 %!in% match1.1]

# Create the matches 
match2.1 <- sample(finalists2, size = 2, replace = FALSE)
match2.2 <- finalists2[finalists2 %!in% match2.1]

### Combine the data -------------

# Load the team members
teams1 <- read.csv("Teams/teams1.csv")
teams2 <- read.csv("Teams/teams2_dag.csv")


# Create the finals data
semis1 <- rbind(match1.1, match1.2) |> as.data.frame()
names(semis1) <- c("Home", "Away") 

# Merge with the team information
semis1 <- left_join(semis1, teams1, by = c("Home" = "team_number")) |> 
  left_join(teams1, by = c("Away" = "team_number"), suffix = c("_home", "_away"))

# Create the finals data
semis2 <- rbind(match2.1, match2.2) |> as.data.frame()
names(semis2) <- c("Home", "Away") 

# Merge with the team information
semis2 <- left_join(semis2, teams2, by = c("Home" = "team_number")) |> 
  left_join(teams2, by = c("Away" = "team_number"), suffix = c("_home", "_away"))


# Write csv
write.csv(semis1, "Teams/semis_league1.csv")
write.csv(semis2, "Teams/semis_league2.csv")


### END ######################################

