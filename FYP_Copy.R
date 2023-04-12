library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)

# Read in the csv file
df <- read.csv("/Users/jackmcfall/Desktop/FYP/Final CSVs/Validated2.0.csv")
df <- arrange(df, Date)
df <- subset(df, select = -year)
df$Date

# Imputing null values in Attendance with 0
unique(df$Attendance)
df$Attendance <- gsub("[^0-9]", "", df$Attendance)
tail(df$Attendance)
df$Attendance <- as.numeric(df$Attendance)

sum(is.na(df$Attendance))
df$Attendance[is.na(df$Attendance)] <- 0
sum(is.na(df$Attendance))
df$Attendance
df$Attendance <- as.numeric(df$Attendance)

# Convert the Date column to a Date type
Date = df$Date <- as.Date(df$Date, "%Y-%m-%d")

# Split the score column into two separate columns for home and away goals
df$NoHomeGoals <- as.numeric(substring(df$Score, 1, 1))
df$NoAwayGoals <- as.numeric(substring(df$Score, 3, 3))

# Replace missing values with 0
df$NoHomeGoals[is.na(df$NoHomeGoals)] <- 0
df$NoAwayGoals[is.na(df$NoAwayGoals)] <- 0

# Create a column to indicate whether each match was a home win, binary
df$home_win <- ifelse(df$NoHomeGoals > df$NoAwayGoals, 1, 0)

# Define the dates that will be used to split the df into 6 subsets
date1 <- as.Date("2018-08-18")
date2 <- as.Date("2020-03-08")
date3 <- as.Date("2021-08-21")
date4 <- as.Date("2021-10-03")
date5 <- as.Date("2022-01-15")
date6 <- as.Date("2022-02-05")
date7 <- as.Date("2022-10-03")

# Split the df into 6 subsets based on the dates
df_Timeline <- split(df, ifelse(df$Date >= date1 & df$Date < date2, 1,
                            ifelse(df$Date >= date2 & df$Date < date3, 2,
                                   ifelse(df$Date >= date3 & df$Date < date4, 3,
                                          ifelse(df$Date >= date4 & df$Date < date5, 4,
                                                 ifelse(df$Date >= date5 & df$Date < date6, 5,
                                                        ifelse((df$Date >= date6), 6, 7)))))))

# Create a vector of phase names
phase_labels <- c("Pre-COVID", "0%", "50%", "75%", "5000 Fans", "Post-COVID")

# Create a vector of colors for each phase
phase_colors <- c("blue", "darkblue", "lightblue", "green", "yellow", "orange")

# Calculate the percentage of home wins for each subset of df
home_wins_perc <- c()

for (i in 1:length(df_Timeline)) {
  df_temp <- df_Timeline[[i]]
  home_wins_perc[i] <- prop.table(table(df_temp$home_win))[2]
}

home_wins_perc

# Create an empty list to store the home team win percentage for each team in each timeline
home_team_win_perc_list <- list()

# Loop through each timeline
for (i in 1:length(df_Timeline)) {
  
  # Subset the data for the current timeline
  df_temp <- df_Timeline[[i]]
  
  # Get a list of all the home teams in the current timeline
  home_teams <- unique(df_temp$Home)
  
  # Create an empty vector to store the home team win percentage for each team in the current timeline
  home_team_win_perc <- rep(NA, length(home_teams))
  
  # Loop through each home team in the current timeline
  for (j in 1:length(home_teams)) {
    
    # Subset the data for the current home team
    df_temp2 <- subset(df_temp, Home == home_teams[j])
    
    # Calculate the home team win percentage for the current home team
    home_team_win_perc[j] <- prop.table(table(df_temp2$home_win))[2]
    
  }
  
  # Combine the home team win percentage vector with the home team names and add it to the list, including phase label
  home_team_win_perc_list[[i]] <- data.frame(Phase = phase_labels[i],
                                             Home = home_teams, HomeWinPerc = home_team_win_perc)
  
}

# Combine the list of home team win percentages into a single data frame
home_team_win_perc_df <- do.call(rbind, home_team_win_perc_list)

# Order the data frame by timeline and then by home team name
home_team_win_perc_df <- home_team_win_perc_df[order(home_team_win_perc_df$Phase, 
                                                     as.numeric(gsub("[^0-9]", "", home_team_win_perc_df$Home))),]

# Create a data frame with the home win percentage for each phase
home_win_df <- data.frame(phase_labels, home_wins_perc)

# Modify the 'phase_labels' column as a factor with the desired order of levels
home_win_df$phase_labels <- factor(home_win_df$phase_labels, levels = phase_labels)

# Plot the bar chart with the new order of phases
ggplot(home_win_df, aes(x = phase_labels, y = home_wins_perc, fill = phase_labels)) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = phase_colors) +
  ylab("Home Win Percentage") + xlab("Phase") + ggtitle("Home Win Rate %") +
  geom_text(aes(label = round(home_wins_perc, digits = 2)), vjust = -0.5, size = 4) +
  coord_cartesian(ylim = c(0.35, 0.425))

# ensuring home win percentages are correct 
date1_win_pct <- mean(subset(df, Date >= date1 & Date < date2)$home_win)
date2_win_pct <- mean(subset(df, Date >= date2 & Date < date3)$home_win)
date3_win_pct <- mean(subset(df, Date >= date3 & Date < date4)$home_win)
date4_win_pct <- mean(subset(df, Date >= date4 & Date < date5)$home_win)
date5_win_pct <- mean(subset(df, Date >= date5 & Date < date6)$home_win)
date6_win_pct <- mean(subset(df, Date >= date6)$home_win)
cat("Home win percentage for pre-covid:", date1_win_pct, "\n")
cat("Home win percentage for 0% fans:", date2_win_pct, "\n")
cat("Home win percentage for 50% fans:", date3_win_pct, "\n")
cat("Home win percentage for 75% fans:", date4_win_pct, "\n")
cat("Home win percentage for 5000 fans:", date5_win_pct, "\n")
cat("Home win percentage for post-covid:", date6_win_pct, "\n")

# Create a new column for quartiles of Distance
df_dist <- df %>% mutate(Distance_quartile = ntile(Distance, 4))
df_dist

# Calculate the home win rate percentage for each quartile range
home_win_perc_dist <- df_dist %>% group_by(Distance_quartile) %>% summarize(home_win_rate = mean(home_win) * 100)

# Print the results
home_win_perc_dist

# Create a bar chart of home win rate percentage by Distance quartile
ggplot(home_win_perc_dist, aes(x = factor(Distance_quartile), y = home_win_rate, fill = factor(Distance_quartile))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  labs(x = "Distance Quartile Range", y = "Home Win Rate Percentage", title = "Home Win Rate Percentage by Distance Quartile") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("[0-25]", "(25-50]", "(50-75]", "(75-100]"))

# Create a new column for quartiles of Distance
df_time <- df %>% mutate(Time_quartile = ntile(Time, 4))

# Calculate the home win rate percentage for each quartile range
home_win_perc_time <- df_time %>% group_by(Time_quartile) %>% summarize(home_win_rate = mean(home_win) * 100)

# Print the results
home_win_perc_time
df_time

# Create a bar chart of home win rate percentage by Time quartile
ggplot(home_win_perc_time, aes(x = factor(Time_quartile), y = home_win_rate, fill = factor(Time_quartile))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  labs(x = "Time Quartile Range", y = "Home Win Rate Percentage", title = "Home Win Rate Percentage by Time Quartile") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("[0-25]", "(25-50]", "(50-75]", "(75-100]"))

# Create a new column 'Phase' based on these dates for model fitting
df$Phase <- factor(ifelse(df$Date >= date1 & df$Date < date2, 1,
                          ifelse(df$Date >= date2 & df$Date < date3, 2,
                                 ifelse(df$Date >= date3 & df$Date < date4, 3,
                                        ifelse(df$Date >= date4 & df$Date < date5, 4,
                                               ifelse(df$Date >= date5 & df$Date < date6, 5,
                                                      ifelse(df$Date >= date6, 6, 7)))))),
                   levels = 1:6, labels = phase_labels)

# Points Calculator
df$Points <- 0
for (i in 1:nrow(df)) {
  score_split <- strsplit(df$Score[i], "-")[[1]]
  home_goals <- as.numeric(score_split[1])
  away_goals <- as.numeric(score_split[2])
  if (home_goals > away_goals) {
    df$Points[i] <- 3
  } else if (home_goals == away_goals) {
    df$Points[i] <- 1
  } else {
    df$Points[i] <- 0
  }
  
}

df$Points

# Create a new column for home team form, initialized with missing values
df$home_team_form <- rep(NA, nrow(df))

start_dates <- c("2018-08-18", "2019-08-24", "2020-09-19", "2021-08-21", "2022-08-13")

# Set the default indicator for newly promoted teams
default_indicator <- 0.4

# Loop through each row in the data frame
for (i in 1:nrow(df)) {

  # Get the home team and date for this row
  home_team <- df$Home[i]
  date <- df$Date[i]
  
  for (j in 1:length(start_dates)) {
    if (date >= start_dates[j]) {
      home_team_in_prev_years <- any(df$Home[df$Date >= start_dates[j] & df$Date < date] == home_team)
        
    }
    if (home_team_in_prev_years) {
      break
    }
  }
  # If the home team is newly promoted, set the home team form to the default indicator
  if (!home_team_in_prev_years) {
    df$home_team_form[i] <- default_indicator
    next
  }

  # Get the previous 6 matches for the home team 
  prev_matches <- df[(df$Home == home_team| df$Away == home_team) & df$Date < date, ]
  prev_matches <- tail(prev_matches, 6)
  
  # Check if the home team's name appears for 20th, 39th, 58th or 77th Time
  # Start of the new season for each team sets to default indicator to account
  # for summer breaks, excludes first 7 home games of season
  home_team_appearances <- sum(df$Home == home_team & df$Date < date)
  if (home_team_appearances %in% c(19, 38, 57, 76) ||
    (home_team_appearances >= 1 && home_team_appearances <= 6) ||
    (home_team_appearances > 19 && home_team_appearances <= 25) ||
    (home_team_appearances > 38 && home_team_appearances <= 44) ||
    (home_team_appearances > 57 && home_team_appearances <= 63) ||
    (home_team_appearances > 76 && home_team_appearances <= 82)) {
    df$home_team_form[i] <- default_indicator
    
  } else {
    # Calculate the home team's win rate in previous 6 matches
    # New indicator based off previous 6 matches
    home_points <- sum(prev_matches$Points)
    df$home_team_form[i] <- home_points / 6
  }
} 

# testing home form indicator 
df$home_team_form
head(df$home_team_form)
head(df)
tail(df$home_team_form)
ac_milan_home_games <- filter(df,(Home == "AC Milan" | Away == "AC Milan"))
ac_milan_home_games
ac_milan_19th_game <- ac_milan_home_games[19, ]
ac_milan_19th_game

# Write the data frame to a CSV file
# write.csv(df, "/Users/jackmcfall/Desktop/FYP/Final CSVs/Finalised_DB.csv", row.names = FALSE)

max(df$Time)
min(df$Time)

# Define the times of day
morning_start <- as.POSIXct("12:30", format = "%H:%M")
morning_start <- format(morning_start, format = "%H:%M")
afternoon_start <- as.POSIXct("16:00", format = "%H:%M")
afternoon_start <- format(afternoon_start, format = "%H:%M")
evening_start <- as.POSIXct("19:00", format = "%H:%M")
evening_start <- format(evening_start, format = "%H:%M")
night_start <- as.POSIXct("21:45", format = "%H:%M")
night_start <- format(night_start, format = "%H:%M")

# Create a vector of labels for the times of day
time_labels <- c("day", "evening", "night")

# Assign the time of day based on the game time
df$TimeOfDay <- factor(
  ifelse(df$Time >= morning_start & df$Time < afternoon_start, time_labels[1],
         ifelse(df$Time >= afternoon_start & df$Time < evening_start, time_labels[2],
                ifelse(df$Time >= evening_start & df$Time <= night_start, time_labels[3], NA))),
  levels = time_labels
)

df$TimeOfDay
table(df$TimeOfDay)

# Create a boxplot of Attendance by Phase
ggplot(df, aes(x = Phase, y = Attendance)) +
  geom_boxplot() +  # or geom_violin() for a violin plot
  labs(x = "Phase", y = "Attendance", title = "Boxplot of Attendance by Phase")

# Perform analysis of variance (ANOVA)
anova_result <- aov(Attendance ~ Phase, data = df)

# Print ANOVA summary
summary(anova_result)

df[df$Phase == "0%", "Attendance", drop = FALSE]
df[df$Phase == "0%" & df$Attendance > 0, ,drop = FALSE]
sum(df$Phase == "0%" & df$Attendance != 0)

# Fit a logistic regression model to home_win with all six subsets as predictors
model <- glm(home_win ~ Phase + I(Distance/5000) + home_team_form + TimeOfDay,
             data = df,family = binomial)

# View the model summary
summary(model)

########################################################################
# Needs fixing up
predict(model)
residuals <- model$residuals
residuals
plot(residuals)

# Calculate hat values
hatvalues <- hatvalues(model)

# Calculate leverage
leverage <- hatvalues / mean(hatvalues)

# Calculate Cook's distance
cooksd <- cooks.distance(model)

# Create a dataframe with the model's residuals and leverage values
influence_df <- influence(model)

# Plot the residuals against the leverage values
plot(influence_df$hat, influence_df$residuals, xlab = "Leverage", 
     ylab = "Residuals", main = "Residuals vs Leverage Values")

# Add a reference line for the Cook's distance cutoff
abline(h = 2 * mean(influence_df$hat) / nrow(df), col = "red")

# Transform the coefficients to odds/probabilities
exp_coefs <- exp(coef(model))
odds <- data.frame(Odds = exp_coefs, 
                   `Probability(%)` = exp_coefs/(1+exp_coefs)*100,
                   `Estimate` = coef(model),
                   `Pr(>|z|)` = summary(model)$coef[, "Pr(>|z|)"],
                   `Std. Error` = summary(model)$coef[, "Std. Error"],
                   `z value` = summary(model)$coef[, "z value"])

# Print the results
odds

# # Interactions
# # Create an interaction between Distance and ss variables
# df$zero_dist <- df$Distance * (df$Phase == "zero_perc")
# df$att_dist <- df$Distance * (df$Attendance)
# df$home_dist <- df$Distance * (df$home_team_form)
# 
# 
# model_zero_dist <- glm(home_win ~ Phase + Distance + zero_dist + I(Attendance/5000) + home_team_form + TimeOfDay,
#                        data = df, family = binomial)
# model_att_dist <- glm(home_win ~ Phase + Distance + att_dist + I(Attendance/5000) + home_team_form + TimeOfDay,
#                       data = df, family = binomial)
# model_home_dist <- glm(home_win ~ Phase + Distance + home_dist + I(Attendance/5000) + home_team_form + TimeOfDay,
#                        data = df, family = binomial)
# summary(model_zero_dist)
# summary(model_att_dist)
# summary(model_home_dist)
# 
# # Create an interaction between TimeofDay and ss variables
# df$zero_time <- (df$Phase == 'zero_perc') * as.numeric(df$TimeOfDay)
# df$att_time <- df$Attendance * as.numeric(df$TimeOfDay)
# df$home_time <- df$home_team_form * as.numeric(df$TimeOfDay)
# 
# model_zero_time <- glm(home_win ~ Phase + Distance + zero_time + I(Attendance/5000) + home_team_form + TimeOfDay,
#                        data = df, family = binomial)
# model_att_time <- glm(home_win ~ Phase + Distance + att_time + I(Attendance/5000) + home_team_form + TimeOfDay,
#                        data = df, family = binomial)
# model_home_time <- glm(home_win ~ Phase + Distance + home_time + I(Attendance/5000) + home_team_form + TimeOfDay,
#                        data = df, family = binomial)
# summary(model_zero_time)
# summary(model_att_time) #only stat sign.
# summary(model_home_time)
# 

# Fit a Poisson regression model to home goals scored with all six subsets as predictors
model_poisson <- glm(NoHomeGoals ~ Phase + I(Distance/5000)
                     + home_team_form + TimeOfDay,
             data = df,family = poisson)

# View the model summary
summary(model_poisson)

# 
# # Create a dataframe with the model's residuals and leverage values
# influence_df1 <- influence(model_poisson)
# 
# # Plot the residuals against the leverage values
# plot(influence_df1$hat, influence_df1$residuals, xlab = "Leverage", 
#      ylab = "Residuals", main = "Residuals vs Leverage Values")
# 
# # Add a reference line for the Cook's distance cutoff
# abline(h = 2 * mean(influence_df1$hat) / nrow(df), col = "red")

# Fit a logistic regression model to the difference in Home Goals Scored vs Away Goals Scored
model_diff_goals <- glm(NoHomeGoals - NoAwayGoals ~ Phase + I(Distance/5000) + home_team_form + TimeOfDay,
                       data = df)

# View the model summary
summary(model_diff_goals)


# Logistic regression model diagnostic checks
plot(residuals(model), type="p", pch=20, xlab="Observation", ylab="Residuals",
     main = "Logistic Regression")
abline(h=0, col="red")

qqnorm(residuals(model))
qqline(residuals(model))

# Residual plot
plot(model, which = 1)

# Q-Q plot of residuals
plot(model, which = 2)

# Scale-location plot
plot(model, which = 3)

# Cook's distance plot
plot(model, which = 4)

# Calculate the AIC and BIC
AIC <- AIC(model)
BIC <- BIC(model)

# Print results
cat("AIC:", AIC, "\n")
cat("BIC:", BIC, "\n")

# Calculate the McFadden's pseudo R-squared
R_squared <- 1 - (model$deviance / model$null.deviance)

# Print result
cat("McFadden's pseudo R-squared:", R_squared, "\n")


# Poisson model diagnostic checks
plot(residuals(model_poisson), type="p", pch=20, xlab="Observation", 
     ylab="Residuals", main = "Poisson")
abline(h=0, col="red")

qqnorm(residuals(model_poisson))
qqline(residuals(model_poisson))

# Residual plot
plot(model_poisson, which = 1)

# Q-Q plot of residuals
plot(model_poisson, which = 2)

# Scale-location plot
plot(model_poisson, which = 3)

# Cook's distance plot
plot(model_poisson, which = 4)

# Calculate the AIC and BIC
AIC <- AIC(model_poisson)
BIC <- BIC(model_poisson)

# Print results
cat("AIC:", AIC, "\n")
cat("BIC:", BIC, "\n")

# Calculate the McFadden's pseudo R-squared
R_squared <- 1 - (model_poisson$deviance / model_poisson$null.deviance)

# Print result
cat("McFadden's pseudo R-squared:", R_squared, "\n")

# Obtain the residual deviance and null deviance
residual_deviance <- summary(model_poisson)$deviance
residual_deviance
null_deviance <- sum(dpois(df$NoHomeGoals, mean(df$NoHomeGoals)))
null_deviance

# Compute the ratio of residual deviance to null deviance
deviance_ratio <- residual_deviance / null_deviance
deviance_ratio

# Predict the probability of a home win for each observation in the dataset
predicted_probs <- predict(model, type = "response")

# Create a data frame with the predicted probabilities and actual outcomes
results <- data.frame(predicted_probs, df$home_win)
tail(results)

# Create a scatter plot
ggplot(results, aes(x = predicted_probs, y = df.home_win)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  xlab("Predicted Probability of Home Win") +
  ylab("Actual Outcome (0 = No Home Win, 1 = Home Win)") +
  ggtitle("Predicted Probabilities vs Actual Outcomes")

ggplot(results, aes(x = predicted_probs, fill = factor(df$home_win))) + 
  geom_histogram(binwidth = 0.05, position = "identity", alpha = 0.7) +
  scale_fill_discrete(name = "Home win") +
  labs(title = "Distribution of predicted probabilities for home wins and losses",
       x = "Predicted probability of home win",
       y = "Count") +
  theme_bw()

