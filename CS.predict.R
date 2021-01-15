library(fplscrapR)
library(tidyverse)
library(skimr)

# Load datasets
# This is obtained from the obtain_player_data.R script

info <- read.csv("player_info.csv")
GWdata <- read.csv("GWdata.csv")
gamelist <- read.csv("gamelist.csv")
fdr <- get_fdr()

# First the game data needs to be rearranged so that every home and away team has a record
# And every record needs the score for and against that team

gl <- gamelist %>%
  pivot_longer(cols = c("home", "away"), values_to = "team", names_to = "team.loc") %>%
  mutate(goals_for = case_when(
    team.loc == "home" ~ team_h_score,
    team.loc == "away" ~ team_a_score
  ),
  goals_ag = case_when(
    team.loc == "home" ~ team_a_score,
    team.loc == "away" ~ team_h_score
  ))

# now we build a table giving the number of clean sheets / number of matches
# this is our probability measure of a clean sheet
# a modelled value for probability of a clean sheet based on goals against
# a weighted average of actual and modelled clean sheet probability is used
# to be agnostic to the possibility of clean sheets being a reliable skill

cs_table <- gl %>%
  filter(finished == TRUE) %>%
  group_by(team) %>%
  summarise(clean_sheet_for = (mean(goals_ag == 0)),
            clean_sheet_ag = (mean(goals_for == 0)),
            mean_goals_ag = mean(goals_ag),
            mean_goals_for = mean(goals_for))

modelled_p_cs <- glm(clean_sheet_for ~ mean_goals_ag, data=cs_table)

cs_table$modelled_cs_for <- (cs_table$clean_sheet_for*0.33) + (modelled_p_cs$fitted.values*0.67)

modelled_p_cs_ag <- glm(clean_sheet_ag ~ mean_goals_for, data=cs_table)

cs_table$modelled_cs_ag <- (cs_table$clean_sheet_ag*0.33) + (modelled_p_cs_ag$fitted.values*0.67)



# Now we build a table of every team against every other team so that we can combine for/against probabilities

teamlist <- as.vector(cs_table$team)

cs_matrix <- crossing(
  team = teamlist,
  opponent = teamlist
)

# We join the tables of clean sheet probability and calculate a summary measure
# Note that an NA is derived when a team plays itself - this is impossible

cs_matrix <- cs_table %>%
  select(team, modelled_cs_for) %>%
  right_join(cs_matrix, by = c("team" = "team"))

cs_matrix <- cs_table %>%
  select(team, modelled_cs_ag) %>%
  right_join(cs_matrix, by = c("team" = "opponent")) %>%
  mutate(cs_prob = ifelse(
    team == team.y, NA,
    (modelled_cs_for + modelled_cs_ag)/2
  ))

# We can show the output in graphical heatmap form

cs_matrix %>%
  ggplot( aes(x = team.y, y = team)) + 
  geom_raster(aes(fill=cs_prob)) + 
  scale_fill_gradient(low="red", high="darkgreen") +
  labs(x="Team", y="Opponent", title="Clean Sheet Probability") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

# We can now build a function to give us the expected clean sheet probability for selected a gameweek/s

GW_cs_prob <- function(wknum){
  
  output <- gamelist %>%
    filter(GW %in% wknum) %>%
    select(home, away, team_h_score, team_a_score)
  
  output <- cs_matrix %>%
    select(cs_prob, team, team.y) %>%
    right_join(output, by = c("team" = "home", "team.y" = "away"))
  
  output <- cs_matrix %>%
    select(cs_prob, team, team.y) %>%
    right_join(output, by = c("team.y" = "team", "team" = "team.y"))
  
  return(output)
    
}

# Here is an example for GW14

GW14 <- GW_cs_prob(c(19:21))

GW14 <- tibble(
  team = c(GW14$team, GW14$team.y),
  cs_prob = c(GW14$cs_prob.y, GW14$cs_prob.x)
) 

GW14 %>%
  group_by(team) %>%
  summarise(cs_prob = sum(cs_prob),
            count = n()) %>%
  ggplot(aes(x=reorder(team, cs_prob), y=cs_prob)) +
  geom_col(aes(fill=factor(count))) +
  coord_flip()

# Estimating player points

GW_cutoff_data <- 14

past_data <- GWdata %>%
  filter(round <= GW_cutoff_data) %>%
  select(minutes, goals_scored, assists, penalties_missed, total_points, bonus, bps, influence, creativity, threat, ict_index, value, playername) %>%
  group_by(playername) %>%
  summarise_all(sum)

past_data <- info %>%
  select(playername, element_type) %>%
  filter(element_type %in% c(3,4)) %>%
  left_join(past_data, by="playername") %>%
  filter(minutes>90)

teamlookup <- tibble(
  team = teamlist,
  team_id = c(1:20)
)

# Round 13 the last when written

training_data <- GWdata %>%
  filter(round > GW_cutoff_data, round <= 17) %>%
  select(playername, opponent_team, total_points, minutes, round) %>%
  right_join(past_data, by = "playername") %>%
  left_join(teamlookup, by=c("opponent_team" = "team_id")) %>%
  left_join(cs_table, by = "team") %>%
  filter(minutes.x > 60)

# sort out this model

model <- lm(total_points.x  ~ influence + creativity + threat + clean_sheet_for, data = training_data)

next_week <- gamelist %>%
  select(team_h, team_a, GW) %>%
  filter(GW %in% c(19,20,21))

next_week <- tibble(
  player_team = c(next_week$team_h, next_week$team_a),
  opponent = c(next_week$team_a, next_week$team_h),
  GW = c(next_week$GW, next_week$GW)
)

next_week <- next_week %>%
  right_join(info, by = c("player_team" = "team")) %>%
  select(playername, opponent, player_team, GW) %>%
  right_join(past_data, by = "playername") %>%
  left_join(teamlookup, by=c("opponent" = "team_id")) %>%
  left_join(cs_table, by = "team") %>%
  filter(element_type %in% c(3,4))

next_week$preds <- predict(model, newdata = next_week)

next_week %>%
  filter(GW %in% c(19)) %>%
  top_n(n=30, wt=preds) %>%
  ggplot(aes(x=reorder(playername, preds), y=preds)) +
  facet_wrap(~GW) +
  geom_col(aes(fill=factor(GW))) +
  coord_flip()

next_week %>%
  ggplot(aes(x=opponent, y=preds)) +
    geom_col(aes(fill = factor(GW)))
