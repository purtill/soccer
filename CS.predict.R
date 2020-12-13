library(fplscrapR)
library(tidyverse)

# Load datasets
# This is obtained from the obtain_player_data.R script

info <- read.csv("player_info.csv")
GWdata <- read.csv("GWdata.csv")
gamelist <- read.csv("gamelist.csv")
fdr <- get_fdr()

# What is the distribution of team goals?

gamelist %>%
  pivot_longer(cols = ends_with("score"), values_to = "goals", names_to = "home.away") %>%
  mutate(
    home.away = case_when(
    home.away == "team_h_score" ~ "home",
    home.away == "team_a_score" ~ "away")
    ) %>%
  ggplot(aes(x = goals)) +
  geom_histogram() +
  facet_grid(rows = vars(home.away))


# match by match comparison

team.perf <- gamelist %>%
  mutate(CSh = (team_a_score == 0),
         CSa = (team_h_score == 0)) %>%
  group_by(home) %>%
  summarise(home.CS = mean(CSh, na.rm=TRUE),
            away.CS = mean(CSa, na.rm=TRUE),
            home.goals = mean(team_h_score, na.rm=TRUE),
            away.goals = mean(team_a_score, na.rm=TRUE))

team.perf %>%
  ggplot(aes(y=away.CS, x=home.goals)) +
  geom_point()
