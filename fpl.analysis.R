library(fplscrapR)
library(data.table)
library(ggplot2)

# Load datasets
# This is obtained from the obtain_player_data.R script

info <- as.data.table(read.csv("player_info.csv"))
GWdata <- as.data.table(read.csv("GWdata.csv"))
gamelist <- as.data.table(read.csv("gamelist.csv"))

# select data for just the top 300 players we can consider as viable options

top300 <- info[order(-total_points),]
top300 <- top300[c(1:300),playername]

# lagged player points

GWdata[, c("lag1", "lag2", "lag3") := shift(total_points, n = 1:3, fill = NA, type = "lag"), by = playername]

# custom FDR

# team list

team.keys <- gamelist[, .SD[1], by=team_h]
team.keys <- team.keys[,list(home,team_h)]
names(team.keys) <- c("team", "team.id")

# comparing points scored against each team

GW.points.ag <- GWdata[,keyby = .(opponent_team,round),.(points.allowed = sum(total_points))]
setnames(GW.points.ag, new = "team.id", old = "opponent_team")
GW.points.ag <- GW.points.ag[team.keys, on = "team.id"]

ave.points.ag <- GW.points.ag[,.(ave=mean(points.allowed)),by=team.id]

current.round <- max(GWdata$round, na.rm = TRUE)

upcoming.weeks <- gamelist[GW %in% c(current.round + 1:5) & !is.na(GW), list(home, away, GW)]
upcoming.weeks.rev <- as.data.table(upcoming.weeks)

upcoming.weeks[, at.home := 1]
upcoming.weeks.rev[, at.home := 0]

setnames(upcoming.weeks, old = c("home", "away"), new = c("team", "opponent"))
setnames(upcoming.weeks.rev, old = c("away", "home"), new = c("team", "opponent"))

upcoming.weeks <- rbind(upcoming.weeks, upcoming.weeks.rev)

upcoming.weeks <- merge.data.table(x = upcoming.weeks, y = ave.points.ag, by.x = "opponent", by.y = "team")
upcoming.weeks <- upcoming.weeks[, exp.points := sum(ave), by = team]

custom.FDR <- upcoming.weeks[, .SD[1], by = team]
custom.FDR <- custom.FDR[,list(team, exp.points)]

ggplot(custom.FDR, aes(x=team, y=exp.points)) + geom_col(aes(fill=exp.points)) + coord_flip()

# comparing players by points per cost

info[, points_per_cost := as.numeric(total_points)/as.numeric(now_cost)]

(p <- 
    ggplot(info[minutes>90], aes(x=as.numeric(now_cost), y=as.numeric(total_points)))
  + geom_point(aes(color=factor(element_type)))
  + geom_text(size = 3, aes(vjust = -1, label = ifelse(web_name %in% c("Bamford", "Vardy", "Son", "Calvert-Lewin", "Kane", "Salah", "Mané", "Jiminez"), web_name, "")))
  + geom_abline(intercept = 0, slope = c(0.1,0.2,0.25,0.33,0.5), alpha = 0.5)
  )
                                                           