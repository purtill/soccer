library(tidyverse)
library(fplscrapR)

gamelist <- read.csv("gamelist.csv")
fdr <- get_fdr()

nextweek <- min(gamelist[gamelist$finished == FALSE,]$GW, na.rm=TRUE)

gametableh <- select(gamelist, home, away, GW) %>%
  spread(key = GW, value = away)

gametablea <- select(gamelist, home, away, GW) %>%
  spread(key = GW, value = home)

for (i in 1:20) {
  for (j in 1:40) {
    
    ifelse(is.na(gametable.h[i,j]),
            gametable.h[i,j] <- as.character(gametable.a[i,j]),
            gametable.h[i,j] <- gametable.h[i,j]
            )
    
  }
}
