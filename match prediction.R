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

## how would going with the bets work out?

set.seed(42)

samp <- sample(25979, 5000)

train <- Match[-samp]
test <- Match[samp]

model <- glm(formula=as.numeric(result==1) ~ B365H, data=train, family=binomial)

pred <- predict.glm(model, newdata = test, type = "response")
