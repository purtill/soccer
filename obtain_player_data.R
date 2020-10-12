library(fplscrapR)

# Get latest FPL data

info <- get_player_info()
GWdata <- get_player_details()
gamelist <- get_game_list()

# Write csv of this data

write.csv(info, file="player_info.csv")
write.csv(GWdata, file="GWdata.csv")
write.csv(gamelist, file="gamelist.csv")