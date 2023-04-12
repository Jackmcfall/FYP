# Read in the csv file
df <- read.csv("/Users/jackmcfall/Desktop/FYP/Final CSVs/Validated2.0.csv")
df <- arrange(df, Date)
# Convert the Date column to a Date type
Date = df$Date <- as.Date(df$Date, "%Y-%m-%d")

# Create a column to indicate whether each match was a home win, binary
df$home_win <- 0
for (i in 1:nrow(df)) {
  score_split <- strsplit(df$Score[i], "-")[[1]]
  home_goals <- as.numeric(score_split[1])
  away_goals <- as.numeric(score_split[2])
  if (home_goals > away_goals) {
    df$home_win[i] <- 1
  }
}

df$home_win
# Define the dates that will be used to split the df into 5 subsets

date1 <- as.Date("2018-08-18")
date2 <- as.Date("2020-03-08")
date3 <- as.Date("2021-08-21")
date4 <- as.Date("2021-10-03")
date5<- as.Date("2022-01-15")
date6 <- as.Date("2022-02-05")
date2018 <- as.Date("2018-08-18")
date2019 <- as.Date("2019-08-24")
date2020 <- as.Date("2020-09-19")
date2021 <- as.Date("2021-08-21")
date2022 <- as.Date("2022-08-13")


# Split the df into 6 subsets based on the dates
df_timeline <- split(df, ifelse(df$Date >= date1 & df$Date < date2, 1,
                                ifelse(df$Date >= date2 & df$Date < date3, 2,
                                       ifelse(df$Date >= date3 & df$Date < date4, 3,
                                              ifelse(df$Date >= date4 & df$Date < date5, 4,
                                                     ifelse(df$Date >= date5 & df$Date < date6, 5,
                                                            ifelse((df$Date >= date6), 6, 7)))))))

# mention in report how it made sense to shorten span of timelines for accuracy
# Calculate the percentage of home wins for each subset of df
home_wins_perc <- c()
for (i in 1:length(df_timeline)) {
  df_temp <- df_timeline[[i]]
  home_wins_perc[i] <- prop.table(table(df_temp$home_win))[2]
}


# Print the results
print(home_wins_perc)

# Create a new column for home team form, initialized with missing values
df$home_team_form <- rep(NA, nrow(df))

start_dates <- c("2018-08-18", "2019-08-24", "2020-09-19", "2021-08-21", "2022-08-13")

# Set the default indicator for newly promoted teams
default_indicator <- 0.5

home_teams <- unique(df$Home)

df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df$home_team_form <- rep(NA, nrow(df))
default_indicator <- 0.5

for (i in 1:nrow(df)) {
  
  home_team <- df$Home[i]
  date <- df$Date[i]
  
  prev_matches <- df[df$Home == home_team & df$Date < date, ]
  home_appearances <- nrow(prev_matches)
  
  # Check if 20, 40, 60, 80 home team appearances have been reached
  if (home_appearances %% 20 == 0) {
    df$home_team_form[df$Home == home_team] <- default_indicator
  }
  
  prev_matches <- tail(prev_matches, 6)
  
  home_wins <- sum(prev_matches$home_win)
  home_losses <- nrow(prev_matches) - home_wins
  df$home_team_form[i] <- home_wins / (home_wins + home_losses)
}

# 
# # Split the df into 4 subsets based on the dates
# df_seasons <- split(df, ifelse(df$Date >= date2018 & df$Date < date2019, 1,
#                                 ifelse(df$Date >= date2019 & df$Date < date2020, 2,
#                                        ifelse(df$Date >= date2020 & df$Date < date2021, 3,
#                                               ifelse(df$Date >= date2021 & df$Date < date2022, 4, 
#                                                      ifelse(df$Date >= date2022,5,6))))))


# # Load the tidyverse and zoo library
# library(tidyverse)
# library(zoo)
# # Create a vector of the 5 dates
# dates <- c("2018-08-18", "2019-08-24", "2020-09-19", "2021-08-21", "2022-08-13")
# 
# for (i in 1:(length(dates) - 1)) {
#   season_df <- df[df$Date >= dates[i + 1] & df$Date < dates[i],]
#   teams <- unique(as.data.frame(season_df$Home))
#   for (j in 1:nrow(teams)) {
#     team_df <- data.frame(season_df[season_df$Home == teams[j,1],])
#     team_df <- team_df[order(team_df$Date),]
#     team_df$cumulative_home_win_pct <- cumsum(team_df$home_win) / 1:nrow(team_df)
#     team_df$rolling_home_win_pct <- rollapply(team_df$cumulative_home_win_pct, 5, mean, align = "right")
#     team_df$form_indicator <- (team_df$rolling_home_win_pct - mean(team_df$rolling_home_win_pct)) / sd(team_df$rolling_home_win_pct)
#     team_df$form_indicator[is.na(team_df$form_indicator)] <- 0
#     print(teams[j,1])
#     print(mean(team_df$form_indicator))
#   }
# }

df
# df$home_team_form <- rep(NA, nrow(df))
# default_indicator <- 0.5
# 
# # List of start dates for each season
# start_dates <- c("2018-08-10", "2019-08-09", "2020-08-08", "2021-08-07", "2022-08-06")
# start_dates <- as.Date(start_dates, format = "%Y-%m-%d")
# 
# # Initialize a counter to track the number of matches for each home team
# home_team_counter <- data.frame(team = unique(df$Home), count = 0)
# 
# for (i in 1:nrow(df)) {
#   
#   home_team <- df$Home[i]
#   date <- df$Date[i]
#   
#   # Check if the home team is newly promoted
#   home_team_in_prev_years <- FALSE
#   for (j in 1:length(start_dates)) {
#     if (date >= start_dates[j]) {
#       home_team_in_prev_years <- any(df$Home[df$Date >= start_dates[j] & df$Date < date] == home_team)
#     }
#     if (home_team_in_prev_years) {
#       break
#     }
#   }
#   if (!home_team_in_prev_years) {
#     df$home_team_form[i] <- default_indicator
#     next
#   }
#   
#   # Get the current count for the home team
#   home_team_count <- home_team_counter[home_team_counter$team == home_team, "count"]
#   if (home_team_count == 0 || home_team_count %% 20 == 0) {
#     df$home_team_form[i] <- default_indicator
#   } else {
#     prev_matches <- df[df$Home == home_team & df$Date < date, ]
#     prev_matches <- tail(prev_matches, 6)
#     
#     home_wins <- sum(prev_matches$home_win)
#     home_losses <- nrow(prev_matches) - home_wins
#     df$home_team_form[i] <- home_wins / (home_wins + home_losses)
#   }
#   
#   # Increment the home team count
#   home_team_counter[home_team_counter$team == home_team, "count"] <- home_team_count + 1
# }

df

df$home_team_form
head(df$home_team_form)
head(df)
tail(df)
ac_milan_home_games <- filter(df, Home == "AS Roma")
ac_milan_home_games
ac_milan_19th_game <- ac_milan_home_games[58, ]
ac_milan_19th_game




