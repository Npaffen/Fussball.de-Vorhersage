## Simulate results of the 19/20 season for Kreisliga Recklinghausen
# starting with the coin flip simulation and the points benchmark method

## 0. Preparation
## 1. simulation
## 2. evaluate result
## 3. calculate plots



## 0. Preparation

require(dplyr)
require(purrr)
source("src/functions.R")

# read in data
dbs<- readRDS(here::here("/data/database_season_1617.rds"))
dbs[,-c(1,5,9)] <- map(dbs[,-c(1,5,9)], as.numeric)
dbm <- readRDS(here::here("/data/database_match_results_1617.rds"))[1:175,]
dbm <- dbm[complete.cases(dbm),]
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
dbs1920 <- dbs[dbs$season == "1617",]



## 1. simulation


# - check games status
games_played <- find_games_played(dbs_data = dbs1920, dbm_data = dbm, season = '1617')
all_games <- make_all_games(dbs_data = dbs1920, games_played)
missing_games <- anti_join(all_games, games_played)
#missing_games2 <- readRDS("data/database_missing_matches_1920.rds")


# repeatedly simulate missing games and calculate average

if(0){
  N <- 5000 # simulation runs
  sim_output <- run_points_sim(missing_games, dbs1920, win_prob_home,
                               sim_results, N, limit = 0.01)
  saveRDS(sim_output, paste0(getwd(), "/data/point_simulation_1617.rds"))
}
sim_output <- readRDS(paste0(getwd(), "/data/point_simulation.rds"))
# simulated data
sim_output$all_final_tables
# convergence plot
sim_output$conv_plot
# missing game results
sim_output$sim_results
# winning probabilities
sim_output$win_prob_home
# average table convergence
sim_output$all_avg_tables



## 2. evaluate result

all_final_tables <- sim_output$all_final_tables
all_final_tables <- add_run_rank_col(x = all_final_tables)
all_avg_tables <- sim_output$all_avg_tables
all_avg_tables <- add_run_rank_col(x = all_avg_tables)

# average table result
average_table <- all_avg_tables[
  (nrow(all_avg_tables)-15):nrow(all_avg_tables),]
print(average_table)
#View(average_table)
points_model_result <- average_table
save(points_model_result, file = "data/points_model_result.rds")


## 3. calculate plots


# compare some clubs
club_names <- c("SV Altendorf-Ulfkotte",
                "SV Schermbeck II",
                "TuS Gahlen",
                "VfL Ramsdorf")

# track one club
club_names <- "VfL Ramsdorf"

# all
club_names <- unique(dbs1920$club_name)

# run plots

# rankings
make_plot(x = all_final_tables, y = "rank",
          club_names, type = "hist")
make_plot(x = all_final_tables, y = "rank",
          club_names, type = "line")

# final table score
make_plot(x = all_final_tables, y = "score",
          club_names, type = "hist")
make_plot(x = all_final_tables, y = "score",
          club_names, type = "line")

# convergence
make_plot(x = all_avg_tables, y = "rank",
          club_names, type = "line")
make_plot(x = all_avg_tables, y = "score",
            club_names, type = "line")


