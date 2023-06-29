### Max-Pong Pairings #################
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
library(httr)
library(rvest)

# Set seed
set.seed(888)

# Create the folder structure
if(!file.exists("Raw")){
dir.create("Raw")
dir.create("Teams")
}


# IMPORTANT:
# The file has to be downloaded and stored in "~/Raw/"

### Load the data -----------------

# Load the data
d <- read_xlsx("Raw/ParticipantsTableTennisTournament.xlsx", skip = 2, col_names = TRUE)

### Data cleaning -------------------

# Clean the names
d <- clean_names(d)

# Clean the skill-levels
d$skill_level <-  tolower(d$skill_level_low_medium_high) |> str_extract("^[a-z]*")

# Specials
low <- c("lowest")
high <- c("guru", "boss")

# Make a new skill level variable
d$skill_level <- ifelse(d$skill_level == low, "low",
       ifelse(d$skill_level %in% high, "high", 
              d$skill_level))

# Correct for those with low self-esteem
d[d$name %in% c("Diego", "Ignacio", "Yuqi") | d$surname %in% c("Theile", "Dierker"), ]$skill_level <- "high"


### Make groupings ------------------

### The goal is to have two equally-sized leagues
### One group will be with high and another with less experienced players
### Group1: less experienced players
### Group2: more experienced players
### People with medium-experience level are assigned randomly to the groups

# Separate the groups
group1 <- subset(d, subset = skill_level == "low")
group2 <- subset(d, subset = skill_level == "high")

# Left
left <- subset(d, subset = skill_level == "medium")
left_group <- 1 + ifelse(runif(n = nrow(left)) >= 0.4, 1, 0)

# Assign
group1 <- bind_rows(group1, left[left_group == 1, ])
group2 <- bind_rows(group2, left[left_group == 2, ])


### Make teams ---------------------

### Function to create pairings
create_double_teams <- function(data) {
  
  # Shuffle the player dataset randomly
  shuffled_players <- data[sample(nrow(data)), ]
  
  # Check if the number of data is even
  if (nrow(shuffled_players) %% 2 != 0) {
    stop("Number of data must be even for creating double teams.")
  }
  
  # Create empty dataframe for double teams
  double_teams <- data.frame(Team1_Player1 = character(),
                             Team1_Player2 = character(),
                             stringsAsFactors = FALSE)
  
  # Create double teams
  for (i in seq(1, nrow(shuffled_players), by = 2)) {
    team1_player1 <- shuffled_players[i, ]
    team1_player2 <- shuffled_players[i + 1, ]
    
    double_teams <- rbind(double_teams,
                          data.frame(Team1_Player1 = team1_player1,
                                     Team1_Player2 = team1_player2,
                                     stringsAsFactors = FALSE))
  }
  
  # Create a team number
  double_teams$team_number <- 1:nrow(double_teams)
  
  return(double_teams)
}

# Remove excess variables
group1 <- subset(group1, select = c(name, surname, email))
group2 <- subset(group2, select = c(name, surname, email))

# Make the teams for group 1
teams1 <- create_double_teams(group1)
teams2 <- create_double_teams(group2)

# Create team names
group1$teams <- paste0("Team ", LETTERS[1:nrow(group1)], "1")
group2$teams <- paste0("Team ", LETTERS[1:nrow(group2)], "2")

### Save the results ------------------------

# Save
write.csv(teams1, "Teams/teams1.csv", fileEncoding = "latin1")
write.csv(teams2, "Teams/teams2.csv", fileEncoding = "latin1")

# -------------------------------------------------------------------------


### END #####################################