## Simulate results of the 19/20 season for Kreisliga Recklinghausen
# starting with the coin flip simulation
# 1. (Weighted) Table Points
# 2. EloRatings.Net
# 3. FIFA Women's World Cup Ranking

## 0. Preparation

require(dplyr)
require(purrr)
source("src/functions.R")

# read in data
dbs<- readRDS(here::here("/data/database_season.rds"))
dbs[,-c(1,5,9)] <- map(dbs[,-c(1,5,9)], as.numeric)
dbm <- readRDS(here::here("/data/database_match_results_1920.rds"))
dbm$matchday <- as.numeric(dbm$matchday)

# clean data
dbs0 <- dbs[0,] # clean frame
for(j in unique(dbs$season)){
  for(i in unique(dbs$club_name)){ # filter out outdated entries
    newest <- filter(dbs, club_name == i, season == j)
    newest <- filter(newest, matchday == max(matchday))
    dbs0 <- bind_rows(dbs0, newest)
  }
}
dbs <- dbs0
dbs1920 <- dbs[dbs$season == "1920",]



## 1. Table Point


# - check games status
games_played <- find_games_played(dbs_data = dbs1920, dbm_data = dbm)
all_games <- make_all_games(dbs_data = dbs1920, games_played)
missing_games <- anti_join(all_games, games_played)
#missing_games2 <- readRDS("data/database_missing_matches_1920.rds")


# - Repeat: simulate missing games

sim_results <- missing_games
win_prob_home <- map2(
  sim_results$club_name_home,
  sim_results$club_name_away,
  ~sim_points(.x, .y, dbs_data = dbs1920)
  )
win_prob_home <- unlist(win_prob_home) # point winning probs

N <- 100 # simulation runs
all_final_tables <- run_points_sim(missing_games, win_prob_home, sim_results, N)
average_table <- aggregate(all_final_tables[,-1],
                           by = list(all_final_tables$club_name),
                           FUN = "mean")



# - evaluate result

View(average_table)