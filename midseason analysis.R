library(fplscrapR)
library(tidyverse)
library(ggrepel)
library(reshape2)

# Load datasets
# This is obtained from the obtain_player_data.R script

info <- read.csv("player_info.csv")
GWdata <- read.csv("GWdata.csv")
gamelist <- read.csv("gamelist.csv")
fdr <- get_fdr()

info %>%
  mutate(element_type = case_when(
    element_type == 1 ~ "GKP",
    element_type == 2 ~ "DEF",
    element_type == 3 ~ "MID",
    element_type ==4 ~ "FWD"
  )) %>%
  count(element_type)


# improving or diminishing form

last.week <- max(GWdata$round)

d <- info %>%
  select(team, playername) %>%
  right_join(GWdata, "playername")

gl <- gamelist %>%
  select(GW, id, team_h, team_a, finished) %>%
  mutate(team = team_a,
         opponent = team_h,
         home = 0)

gl <- gamelist %>%
  select(GW, id, team_h, team_a, finished) %>%
  mutate(team = team_h,
         opponent = team_a,
         home = 1) %>%
  rbind(gl)

gl <- gl %>%
  select(-c(team_h, team_a))

gl <- fdr %>%
  select(id,
         name,
         short_name,
         strength,
         strength_overall_home,
         strength_overall_away) %>%
  right_join(gl, by = c("id" = "opponent")) %>%
  filter(GW > last.week - 5 & GW <= last.week + 5)

fd.change <- dcast(gl, team~finished, value.var='strength_overall_home', mean)

fd.change <- fdr %>%
  select(short_name, id) %>%
  right_join(fd.change, by = c("id" = "team"))

dif.shift <- fd.change %>%
  set_names(c("team", "id", "lead", "lag")) %>%
  mutate(shift = lead - lag)

dif.shift %>%
  ggplot(aes(x=reorder(team, -shift), y=shift)) +
  geom_col() +
  coord_flip()


# Connecting our fixture difficulty measure with player form

form <- GWdata %>%
  filter(round >= last.week -5 & round <= last.week & minutes > 0) %>%
  group_by(playername) %>%
  summarise(form = mean(total_points),
            mins = sum(minutes),
            ict_index = mean(ict_index),
            clean_sheets = sum(clean_sheets),
            goal_actions = sum(goals_scored) + sum(assists))

form <- info %>%
  select(element_type, now_cost, playername, team) %>%
  right_join(form, by = "playername")

form <- dif.shift %>%
  select(id, team, shift) %>%
  right_join(form, by = c("id" = "team"))

form %>%
  filter(form >= 4, mins > 180) %>%
  ggplot(aes(x=now_cost, y=form)) +
  geom_point(aes(color=-shift)) +
  scale_colour_gradient2(low = 'red', mid = 'gray50', high = 'green') +
  geom_text_repel(aes(label = ifelse(form > 5, as.character(playername),""))) +
  facet_wrap(~element_type) + 
  theme_bw()

