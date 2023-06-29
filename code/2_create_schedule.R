### Tournmenet schedule ###############
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
library(lubridate)
library(TouRnament)

# Set seed
set.seed(888)

# Create a not-in funciton
`%notin%` <- Negate(`%in%`)

## Load the groups
group1 <- read.csv("Teams/teams1.csv")
group2 <- read.csv("Teams/teams2_dag.csv")


# Create team names
group1$teams <- paste0("Team ", LETTERS[1:nrow(group1)], "1")
group2$teams <- paste0("Team ", LETTERS[1:nrow(group2)], "2")


# Define weekend
weekend <- c("Saturday", "Sunday")

# Specify the dates
final <- dmy("12.07.2023")
start <- dmy("18.06.2023")
end   <- dmy("09.07.2023")

# Duration
difftime(end, start)

# Get the days
days <- seq(start, end, by = "days")
workdays <-  days[weekdays(days) %notin% weekend]

# Get the weeks
weeks <- unique(isoweek(days))

# Estimate the number of weeks
num_weeks <- length(unique(weeks))

### Assign the pairings ----------------------------

### Create the tournament schedule
schedule1 <- roundrobin(group1$teams, second_round = FALSE)
schedule2 <- roundrobin(group2$teams, second_round = FALSE)

# Filter the first two matchdays
schedule1 <- subset(schedule1, subset = Matchday %in% c(1,2))
schedule2 <- subset(schedule2, subset = Matchday %in% c(1,2))

### Check whether every time has about equal games
schedule_long1 <- schedule1 |>
  pivot_longer(cols = c("Home", "Away"), names_to = "Place", values_to = "Team") |>
  group_by(Team) |>
  count()
schedule_long2 <- schedule2 |>
  pivot_longer(cols = c("Home", "Away"), names_to = "Place", values_to = "Team") |>
  group_by(Team) |>
  count()

# Function to create balanced leagues
complete_league <- function(league = 1) {
  tmp <- get(paste0("schedule_long", league))
  if (all(tmp$n == 2)) {
    cat("The league is balanced!")
  } else {
    remainder <- tmp$Team[tmp$n == 1]
    n <- length(remainder)
  if (n %% 2 != 0) n <- n - 1
    games <- vector("character", length = n)
    for (i in 1:n) {
    games[i] <- remainder[i]
    }
    # Create a data frame
    data <- data.frame(Matchday = 2,
                       Home     = games[1:n %% 2 != 0],
                       Away     = games[1:n %% 2 == 0])
  return(data)
  }
}

# Complete the league
complete_league(league = 1)
league2_add <- complete_league(league = 2)

# Combine the data
schedule2 <- bind_rows(schedule2, league2_add)

# Remove unimportant variables
group1 <- subset(group1,
                 select = c("teams", "Team1_Player1.name", "Team1_Player1.surname", "Team1_Player2.name",  "Team1_Player2.surname"))
group2 <- subset(group2,
                 select = c("teams", "Team1_Player1.name", "Team1_Player1.surname", "Team1_Player2.name",  "Team1_Player2.surname"))


# Left join with teams
schedule1 <- left_join(schedule1, group1, by = c("Home" = "teams")) |> 
  left_join(group1, by = c("Away" = "teams"), suffix = c("_home", "_away"))
schedule2 <- left_join(schedule2, group2, by = c("Home" = "teams")) |> 
  left_join(group2, by = c("Away" = "teams"), suffix = c("_home", "_away"))

# Add the columns for winning and losing team
schedule1$Winner <- schedule2$winner <- "Win"
schedule1$Loser  <- schedule2$Loser  <- "Lost" 


# Export the results
write.csv(schedule1, file = "Teams/schedule1.csv", fileEncoding = "latin1")
write.csv(schedule2, file = "Teams/schedule2.csv", fileEncoding = "latin1")


### END #############################