library(RSQLite)
library(data.table)
library(ggplot2)

con <- dbConnect(RSQLite::SQLite(), dbname="~/Desktop/soccer.sqlite")

tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

DataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  DataFrames[[i]] <- dbGetQuery(conn=con, 
  statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}

dbDisconnect(con)

## Transform into single data frams - beware awful code ahead

Country <- as.data.table(DataFrames[1])
League <- as.data.table(DataFrames[2])
Match <- as.data.table(DataFrames[3])
Player <- as.data.table(DataFrames[4])
Player_Attributes <- as.data.table(DataFrames[5])
Team <- as.data.table(DataFrames[6])
Team_Attributes <- as.data.table(DataFrames[7])

rm(tables)
rm(DataFrames)

# Add result variable based on goals

Match[,result := sign(home_team_goal - away_team_goal)]

results <- Match[, .N , by=result]
results[,N := N/sum(N)]

## plot results by betting

(p <- ggplot(data=Match, aes(x=B365H, y=as.numeric(result==1)))
  + geom_point()
  + geom_smooth(method = "glm", 
                method.args = list(family = "binomial"), 
                se = FALSE)
  + scale_x_continuous(trans='log2'))

## join team names

Match <- merge(x = Match, y= Team[,c("team_api_id", "team_long_name")], by.x="home_team_api_id", by.y = "team_api_id", all.x=TRUE)
setnames(Match, "team_long_name", "home_team_name")

Match <- merge(x = Match, y= Team[,c("team_api_id", "team_long_name")], by.x="away_team_api_id", by.y = "team_api_id", all.x=TRUE)
setnames(Match, "team_long_name", "away_team_name")

head(Match[home_team_name=="Chelsea",])

## Focus on English matches

eng_match <- Match[country_id==1729,]

epl1516 <- eng_match[season=="2015/2016"]

ggplot(data=eng_match, aes(x=away_team_name, y=home_team_name)) +
  geom_point(aes(color=as.factor(result))) +
  scale_color_hue()

arsenalseason <- epl1516[(home_team_name=="Arsenal" | away_team_name=="Arsenal"),c("home_team_name", "away_team_name","stage","date", "result")]
setkey(arsenalseason, stage)

ggplot(arsenalseason, aes(x=stage, y=1)) + geom_point(aes(color=(as.factor((-2*as.numeric(away_team_name=="Arsenal")+1)*result))))
