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
dbs1920 <- dbs[dbs$season == "1920",]



## 1. Table Point


# - check games status
games_played <- find_games_played(dbs_data = dbs1920, dbm_data = dbm)
all_games <- make_all_games(dbs_data = dbs1920, games_played)
missing_games <- anti_join(all_games, games_played)
#missing_games2 <- readRDS("data/database_missing_matches_1920.rds")


# repeatedly simulate missing games and calculate average

if(0){
  N <- 5000 # simulation runs
  sim_output <- run_points_sim(missing_games, dbs1920, win_prob_home,
                               sim_results, N, limit = 0.01)
  saveRDS(sim_output, paste0(getwd(), "/data/point_simulation.rds"))
}
sim_output <- loadRDS(paste0(getwd(), "/data/point_simulation.rds"))
# simulated data
all_final_tables <- sim_output[[1]] 
# convergence plot
sim_output[[2]]
# missing game results
sim_output[[3]] 
# winning probabilities
cbind(missing_games,sim_output[[4]])
# average rable results
average_table <- aggregate(all_final_tables[,-1],
                           by = list(all_final_tables$club_name),
                           FUN = "mean")
# - evaluate result

print(average_table)


# calculate ranking plot

all_final_tables <- sim_output[[1]] 
all_final_tables$run <- rep(1:(length(all_final_tables$score)/16), each = 16)
all_final_tables$rank <- NA

for(i in 1:(length(all_final_tables$score)/16)){
  tab <- all_final_tables[all_final_tables$run == i,]
  tab <- transform(tab, rank = rank(-score, ties.method = "first"))
  all_final_tables[all_final_tables$run == i,] <- tab
}

df <- all_final_tables
df <- df[,c("club_name","run","rank")]
#df <- group_by(df, "club_name")

# plot selection
dfa <- filter(df, club_name %in% c("VfL Ramsdorf","TuS Gahlen", "SV Schermbeck II", "	SV Altendorf-Ulfkotte"))
# rank as lines
rank_plot <- ggplot(dfa, aes(x=run, y=rank, colour = club_name))
rank_plot + geom_line()
# rank as histogram
dfa <- filter(df, club_name %in% c("VfL Ramsdorf"))
rank_plot <- ggplot(dfa, aes(x = factor(rank)))
rank_plot + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title= "VfL Ramsdorf ranking distribution")
# all ranks hist
dfa <- filter(df, club_name %in%
                c("VfL Ramsdorf","TuS Gahlen", "SV Schermbeck II",
                  "SV Altendorf-Ulfkotte"))
ggplot(dfa,aes(x=rank, fill=club_name)) + geom_histogram(alpha=0.25, binwidth = 1)
ggplot(df,aes(x=rank, fill=club_name)) + geom_histogram(alpha=0.25, binwidth = 1)
