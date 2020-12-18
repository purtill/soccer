library(fplscrapR)
library(tidyverse)

# Load datasets
# This is obtained from the obtain_player_data.R script

info <- read.csv("player_info.csv")
GWdata <- read.csv("GWdata.csv")
gamelist <- read.csv("gamelist.csv")
fdr <- get_fdr()

# What is the distribution of team goals?

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

GW_limit <- 10

cs_table <- gl %>%
  filter(finished == TRUE, GW <= GW_limit) %>%
  group_by(team) %>%
  summarise(clean_sheet_for = (0.8*mean(goals_ag == 0))+(0.2*0.25),
            clean_sheet_ag = (0.8*mean(goals_for == 0))+(0.2*0.25))

teamlist <- as.vector(cs_table$team)

cs_matrix <- crossing(
  team = teamlist,
  opponent = teamlist
)

cs_matrix <- cs_table %>%
  select(team, clean_sheet_for) %>%
  right_join(cs_matrix, by = c("team" = "team"))

cs_matrix <- cs_table %>%
  select(team, clean_sheet_ag) %>%
  right_join(cs_matrix, by = c("team" = "opponent")) %>%
  mutate(cs_prob = ifelse(
    team == team.y, NA,
    (clean_sheet_for + clean_sheet_ag)/2
  ))

cs_matrix %>%
  ggplot( aes(x = team.y, y = team)) + 
  geom_raster(aes(fill=cs_prob)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="Team", y="Opponent", title="Clean Sheet Probability") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

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

GW14 <- GW_cs_prob(c(11:13))

GW14 <- tibble(
  team = c(GW14$team, GW14$team.y),
  cs_prob = c(GW14$cs_prob.y, GW14$cs_prob.x)
) 

GW14 %>%
  ggplot(aes(x=reorder(team, cs_prob), y=cs_prob)) +
  geom_col() +
  coord_flip()


