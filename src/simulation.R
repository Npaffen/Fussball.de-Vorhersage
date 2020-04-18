## Simulate results of the 19/20 season for Kreisliga Recklinghausen
# starting with the coin flip simulation
# 1. (Weighted) Table Points
# 2. EloRatings.Net
# 3. FIFA Women's World Cup Ranking

## 0. Preparation

require(dplyr)

# read in and clean data
database_season <- readRDS(here::here("/data/database_season.rds"))
clubs <- unique(database_season$club_name)
dbs <- database_season[0,] # clean frame
for(i in clubs){ # filter out outdated entries
  newest <- filter(database_season, club_name == i)
  newest <- filter(newest, games == max(games))
  dbs <- bind_rows(dbs, newest)
}
database_match_results <- readRDS(here::here("/data/database_match_results.rds"))
dbs1920 <- dbs[dbs$season == "1920",]



## 1. Table Point

# - list clubs
# - get table points

# - check games status
# - Repeat: simulate missing games
# - evaluate result

