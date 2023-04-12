# Read in the csv file
df <- read.csv("/Users/jackmcfall/Desktop/FYP/Final CSVs/Validated2.0.csv")

# Convert the Date column to a Date type
df$Date <- as.Date(df$Date, "%Y-%m-%d")

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

df$home_goals
df$away_goals
df$home_win
# Define the dates that will be used to split the df into 5 subsets

date1 <- as.Date("2018-08-18")
date2 <- as.Date("2020-03-08")
date3 <- as.Date("2021-08-21")
date4 <- as.Date("2021-10-03")
date5<- as.Date("2022-01-15")
date6 <- as.Date("2022-02-05")
start_dates <- c("2018-08-18", "2019-08-24", "2020-09-19", "2021-08-21", "2022-08-13")

# Split the df into 5 subsets based on the dates
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

# calculate_form <- function(df, n) {
#   form <- numeric(nrow(df))
#   for (i in n:nrow(df)) {
#     home_wins <- sum(df$Home[(i-n+1):i] == "Home" & df$Score[(i-n+1):i] > 0)
#     total_matches <- n
#     form[i] <- home_wins / total_matches
#   }
#   df$form <- form
#   return(df)
# }
#       
# df$form

# library(dplyr)
# 
# # function to calculate points based on result
# calculate_points <- function(home_goals, away_goals) {
#   if (home_goals > away_goals) {
#     return(3)
#   } else if (home_goals == away_goals) {
#     return(1)
#   } else {
#     return(0)
#   }
# }

# # loop through all matches
# for (i in 1:nrow(df)) {
#   df$points[i] <- calculate_points(home_goals, away_goals)
# }
# 
# # create a rolling sum of the points
# df <- df %>%
#   group_by(Home) %>%
#   mutate(rolling_points = cumsum(points),
#          home_form = sum(points, na.rm=TRUE) / 6) %>%
#   ungroup()

# fill NAs in rolling_points with 0
# df$rolling_points[is.na(df$rolling_points)] <- 0
# 
# # replace NAs in home_form with 0
# df$home_form[is.na(df$home_form)] <- 0
# head(df$home_form)
# head(df$points)
# head(df$rolling_points)
# head(df$Home)
# tail(df$Home)
# tail(df$rolling_points)
# df


df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
df$home_team_form <- rep(NA, nrow(df))
default_indicator <- 0.5

for (i in 1:nrow(df)) {
  
  home_team <- df$Home[i]
  date <- df$Date[i]
  
  # Check if the home team is newly promoted
  home_team_in_prev_years <- FALSE
  for (j in 1:length(start_dates)) {
    if (date >= start_dates[j]) {
      home_team_in_prev_years <- any(df$Home[df$Date >= start_dates[j] & df$Date < date] == home_team)
    }
    if (home_team_in_prev_years) {
      break
    }
  }
  if (!home_team_in_prev_years) {
    df$home_team_form[i] <- default_indicator
    next
  }
  
  prev_matches <- df[df$Home == home_team & df$Date < date, ]
  prev_matches <- tail(prev_matches, 6)
  
  home_wins <- sum(prev_matches$home_win)
  home_losses <- nrow(prev_matches) - home_wins
  df$home_team_form[i] <- home_wins / (home_wins + home_losses)
}

df$home_team_form
head(df$home_team_form)
head(df)
tail(df)
ac_milan_home_games <- filter(df, Home == "AS Roma")
ac_milan_home_games
