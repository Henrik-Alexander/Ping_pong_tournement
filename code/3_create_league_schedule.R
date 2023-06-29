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


create_tournament_schedule <- function (teamvector, randomize = TRUE, seed = 999, weeks = weeks){
  try(if (length(teamvector) < 5) 
    stop("number of teams has to be at least 5"))
  try(if (typeof(randomize) != "logical") 
    stop("randomize has to be logical"))
  if (missing(seed) == FALSE) {
    try(if (typeof(seed) != "double") 
      stop("seed has to be an integer"))
  }
  if (length(teamvector)%%2 == 1) {
    teamvector <- append(teamvector, "free")
  }
  if (randomize == TRUE) {
    if (missing(seed) == FALSE) {
      set.seed(seed)
      teamvector <- sample(teamvector)
    }
    if (missing(seed) == TRUE) {
      teamvector <- sample(teamvector)
    }
  }
  teams <- as.integer(1:length(teamvector))
  teamid <- cbind(teamvector, teams)
  for (day in 1:(length(teams)-1)) {
    if (day == 1) {
      schedule <- data.frame(Team1 = integer(), Team2 = integer(), 
                             Game = integer(), stringsAsFactors = FALSE)
      up <- teams[2:((length(teams)/2))]
      down <- teams[(length(teams) - 1):((length(teams)/2) + 
                                           1)]
      left1 <- teams[1]
      left2 <- length(teams)
    }
    else {
      left2old <- left2
      left2 <- up[1]
      up <- append(up[2:length(up)], down[length(down)])
      down <- append(left2old, down[1:(length(down) - 1)])
    }
    rows <- nrow(schedule)
    schedule[(rows + 1):(rows + length(up)), "Team1"] <- up
    schedule[(rows + 1):(rows + length(down)), "Team2"] <- down
    schedule[(rows + length(up) + 1), "Team1"] <- left1
    schedule[(rows + length(up) + 1), "Team2"] <- left2
    schedule[(rows + 1):(rows + (length(teams)/2)), "Game"] <- day
  }
  schedule$HA <- ifelse(schedule$Game%%2 == 0, 
                        ifelse((schedule$Team1%%2 ==  1 & schedule$Team1 > schedule$Game) | (schedule$Team1%%2 ==  0 & schedule$Team1 <= schedule$Game), 1, 2), 
                        ifelse((schedule$Team1%%2 == 0 & schedule$Team1 > schedule$Game) | (schedule$Team1%%2 == 1 & schedule$Team1 < schedule$Game), 1, 2))
  schedule$HA <- ifelse(schedule$Game == 1, ifelse((schedule$Team1%%2 == 0), 1, 2), schedule$HA)
  schedule$HA <- ifelse(schedule$Game == 2, ifelse((schedule$Team1%%2 ==  1), 1, 2), schedule$HA)
  schedule$HomeID <- ifelse(schedule$HA == 1, schedule$Team1, 
                            schedule$Team2)
  schedule$AwayID <- ifelse(schedule$HA == 1, schedule$Team2, 
                            schedule$Team1)
  home <- merge(schedule, teamid, by.x = "HomeID", by.y = "teams", 
                all.x = TRUE)
  schedule <- merge(home, teamid, by.x = "AwayID", by.y = "teams", 
                    all.x = TRUE)
  rm(home)
  schedule$HA <- NULL
  schedule$Team1 <- NULL
  schedule$Team2 <- NULL
  schedule$HomeID <- NULL
  schedule$AwayID <- NULL
  names(schedule) <- c("Game", "Home", "Away")
  schedule <- schedule[order(schedule$Game), ]
  row.names(schedule) <- 1:nrow(schedule)
  schedule <- schedule[order(schedule$Home, schedule$Away), ]
  schedule$Week <- rep(weeks, length = nrow(schedule))
  schedule <- schedule[order(schedule$Week), c("Week", "Home", "Away")]
  
  return(schedule)
}




### Create the tournament schedule
schedule1 <- create_tournament_schedule(group1$teams, weeks = weeks)
schedule2 <- create_tournament_schedule(group2$teams, weeks = weeks)

### Check whether every time has about equal games
schedule_long1 <- schedule1 |> pivot_longer(cols = c("Home", "Away"), names_to = "Place", values_to = "Team") |> group_by(Team, Week) |> count()
schedule_long2 <- schedule2 |> pivot_longer(cols = c("Home", "Away"), names_to = "Place", values_to = "Team") |> group_by(Team, Week) |> count()

# Remove unimportant variables
group1 <- subset(group1, select = c("teams", "Team1_Player1.name", "Team1_Player1.surname", "Team1_Player2.name",  "Team1_Player2.surname"))
group2 <- subset(group2, select = c("teams", "Team1_Player1.name", "Team1_Player1.surname", "Team1_Player2.name",  "Team1_Player2.surname"))


# Left join with teams
schedule1 <- left_join(schedule1, group1, by = c("Home" = "teams")) |> 
  left_join(group1, by = c("Away" = "teams"), suffix = c("_home", "_away"))
schedule2 <- left_join(schedule2, group2, by = c("Home" = "teams")) |> 
  left_join(group2, by = c("Away" = "teams"), suffix = c("_home", "_away"))



# Export the results
write.csv(schedule1, file = "Teams/schedule1.csv")
write.csv(schedule2, file = "Teams/schedule2.csv")


### END #############################