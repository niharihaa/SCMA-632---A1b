# Install and load the fitdistrplus package
install.packages("fitdistrplus")
library(fitdistrplus)

# Load necessary libraries
library(dplyr)

# Load the dataset
ipl_data <- read.csv("C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/Assignments/A1b/IPL_ball_by_ball_updated till 2024.csv")

# Aggregate data season-wise, batsman-wise, and bowler-wise
ipl_summary <- ipl_data %>%
  group_by(Season, Match.id, Striker, Bowler) %>%
  summarise(
    total_runs = sum(runs_scored, na.rm = TRUE),
    total_wickets = sum(wicket_confirmation, na.rm = TRUE),
    .groups = "drop"
  )

# Top three run-getters and top three wicket-takers per season
top_players_per_season <- ipl_summary %>%
  group_by(Season) %>%
  summarise(
    top_run_getters = list(head(arrange(ipl_summary, desc(total_runs)), 3)),
    top_wicket_takers = list(head(arrange(ipl_summary, desc(total_wickets)), 3)),
    .groups = "drop"
  )

# Filter data for the last three IPL tournaments
last_three_seasons <- ipl_data %>%
  filter(Season %in% tail(unique(Season), 3))

# Get top three batsmen based on total runs
top_batsmen <- last_three_seasons %>%
  group_by(Striker) %>%
  summarise(total_runs = sum(runs_scored, na.rm = TRUE)) %>%
  arrange(desc(total_runs)) %>%
  head(3)

# Get top three bowlers based on total wickets
top_bowlers <- last_three_seasons %>%
  group_by(Bowler) %>%
  summarise(total_wickets = sum(wicket_confirmation, na.rm = TRUE)) %>%
  arrange(desc(total_wickets)) %>%
  head(3)

# Extract the data for top batsmen and bowlers
top_batsmen_runs <- last_three_seasons %>%
  filter(Striker %in% top_batsmen$Striker) %>%
  pull(runs_scored)

top_bowlers_wickets <- last_three_seasons %>%
  filter(Bowler %in% top_bowlers$Bowler) %>%
  pull(wicket_confirmation)

# Function to fit and plot distribution
fit_and_plot_distribution <- function(data) {
  fit <- fitdist(data, "norm")
  plot(fit)
  return(fit)
}

# Fit distributions for top batsmen and bowlers
fit_batsmen <- fit_and_plot_distribution(top_batsmen_runs)
fit_bowlers <- fit_and_plot_distribution(top_bowlers_wickets)

# Summary of fitted distributions
summary(fit_batsmen)
summary(fit_bowlers)

# Filter data for SP Narine
narine_data <- ipl_data %>%
  filter(Striker == "SP Narine" | Bowler == "SP Narine")

install.packages("readxl")
library(readxl)

# Load the salary dataset
salary_data <- read_excel("C://Users//nihar//OneDrive//Desktop//Bootcamp//SCMA 632//Assignments//A1b//IPL SALARIES 2024.xlsx")

# View the structure and first few rows of the salary dataset to identify relevant columns
str(salary_data)
head(salary_data)

# Filter the salary data for SP Narine
narine_salary <- salary_data %>%
  filter(grepl("Sunil Narine", Player))

# Check unique player names to ensure correct filtering
unique(salary_data$Player)

# Filter the salary data for SP Narine
narine_salary <- salary_data %>%
  filter(grepl("Sunil Narine", Player))

# Check the selected data
print(narine_salary)

# Manually create the salary data for SP Narine
seasons <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021, 2022, 2023, 2024)
salaries <- c(600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600, 600) 
narine_salary <- data.frame(
  Player = rep("Sunil Narine", length(seasons)),
  Season = seasons,
  Salary = salaries
)

# Print the manually created salary data
print(narine_salary)

# Summarize performance metrics for SP Narine
narine_performance <- narine_data %>%
  group_by(Season) %>%
  summarise(
    total_runs = sum(ifelse(Striker == "SP Narine", runs_scored, 0), na.rm = TRUE),
    total_wickets = sum(ifelse(Bowler == "SP Narine", wicket_confirmation, 0), na.rm = TRUE)
  )

# Clean the Season column to retain only numeric values and then convert to numeric
narine_salary$Season <- as.numeric(gsub("[^0-9]", "", narine_salary$Season))
narine_performance$Season <- as.numeric(gsub("[^0-9]", "", narine_performance$Season))

# Ensure there are no NAs in Season columns after conversion
narine_salary <- narine_salary %>%
  filter(!is.na(Season))
narine_performance <- narine_performance %>%
  filter(!is.na(Season))

# Join the summarized performance metrics with the salary data
narine_performance <- narine_performance %>%
  left_join(narine_salary, by = "Season")

# Ensure the join is correct
print(narine_performance)

# Remove rows with NA values in the narine_performance data frame
narine_performance <- narine_performance %>%
  filter(!is.na(Player) & !is.na(Salary))

# Fit a linear model to find the relationship between performance and salary
fit_model_runs <- lm(Salary ~ total_runs, data = narine_performance)
fit_model_wickets <- lm(Salary ~ total_wickets, data = narine_performance)

# Summary of the models
summary(fit_model_runs)
summary(fit_model_wickets)

# Plot the relationship
plot(narine_performance$total_runs, narine_performance$Salary, main = "Salary vs Runs", xlab = "Total Runs", ylab = "Salary")
abline(fit_model_runs, col = "blue")
plot(narine_performance$total_wickets, narine_performance$Salary, main = "Salary vs Wickets", xlab = "Total Wickets", ylab = "Salary")
abline(fit_model_wickets, col = "red")