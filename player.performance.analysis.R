library(fplscrapR)
library(data.table)
library(ggplot2)

# Get latest FPL data

info <- as.data.table(get_player_info())

# Create points/spend var

info <- info[, points_per_spend := as.numeric(points_per_game)/as.numeric(now_cost)]

# Create plot

(playerplot <- ggplot(info[minutes>91], aes(x=as.numeric(now_cost), y=as.numeric(points_per_game))) +
                        geom_point(aes(colour=points_per_spend)))
  