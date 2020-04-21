library(readr)
library(tidyverse)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results_1920.rds", sep = "/"))



database_season <- readRDS(here::here("/data/database_season.rds"))  
#Idee für Untenschieden : Ausrechen wie häufig unentschieden in dieser Saison gespielt wurde. 
#Annahme : Zwei gleichstarke Mannschaften haben eine höhere Wahrscheinlichkeit unentschieden zu spielen als zwei unterschiedlich Starke.
#Ableitung : Für gleichstarke Mannschaften Durchschnittswert für unterschiedlich Starke den Wert diskontieren 
#Proportion zum Rankingunterschied. 

















#create ratings for the first 20 matchdays
for (i in seq_along(database_mr$season)) {
  rating <- f_rating_update(rating = rating,
                            club_name_home = database_mr$club_name_home[i],
                            club_name_away = database_mr$club_name_away[i],
                            K = database_mr$K[i],
                            W_team_home = database_mr$W_team_home[i],
                            W_team_away = database_mr$W_team_away[i]
                            )
}
rating <- rating %>% arrange(desc(rating_value))
db_missing_games <- read_rds(here::here("/data/database_missing_matches_1920.rds"))


db_missing_games <- db_missing_games %>% mutate(K = 30)
points_last_md <- database_season %>% filter(matchday == 20, season == 1920) %>% rename(teams = club_name, points_chr = points) %>%
  mutate(points = as.numeric(points_chr))  %>%
  select(teams,points)
  


#Tabelle für das Rating der simulierten Spiele
rating_sim_reset <- rating %>% select(teams, rating_value) %>% mutate(rating_old = rating_value)
#Tabelle um Gewinne zu zählen
chart_prognose_reset <-  tibble(teams = rating_sim_reset$teams) %>% inner_join( points_last_md)

cp_20000 <- list()
rating_sim_2000 <- list()


for (l in 1:200000){
  chart_prognose <- chart_prognose_reset
  rating_sim <- rating_sim_reset
  for (j in unique(db_missing_games$matchday)){
db_missing_matchday <- db_missing_games %>% filter( matchday == j)
rating_sim$rating_old <- rating_sim$rating_value

for (i in seq_along(db_missing_matchday$matchday)) {
  
  rating_sim <- f_match_simulation(rating = rating_sim,
                            club_name_home = db_missing_matchday$club_name_home[i],
                            club_name_away = db_missing_matchday$club_name_away[i],
                            K = db_missing_matchday$K[i]
                            
                            
  )
}

for (m in seq_along(chart_prognose$teams)){
chart_prognose$points[m] <- if (rating_sim$rating_value[m] > rating_sim$rating_old[m]) {chart_prognose$points[m] +3} else chart_prognose$points[m]

}
}
cp_20000[[l]] <- chart_prognose %>% arrange(desc(points)) %>% mutate(rank = 1:dim(.)[1] )
rating_sim_2000[[l]] <- rating_sim
}

cp_200000_no_update <- list()
for (l in 1:200000){
  chart_prognose <- chart_prognose_reset
  
  for (j in unique(db_missing_games$matchday)){
    db_missing_matchday <- db_missing_games %>% filter( matchday == j)

    rating_sim <- rating_sim_reset
    for (i in seq_along(db_missing_matchday$matchday)) {
      
      rating_sim <- f_match_simulation(rating = rating_sim,
                                       club_name_home = db_missing_matchday$club_name_home[i],
                                       club_name_away = db_missing_matchday$club_name_away[i],
                                       K = db_missing_matchday$K[i]
                                       
                                       
      )
    }
    
    for (m in seq_along(chart_prognose$teams)){
      chart_prognose$points[m] <- if (rating_sim$rating_value[m] > rating_sim$rating_old[m]) {chart_prognose$points[m] +3} else chart_prognose$points[m]
      
    }
  }
  cp_200000_no_update[[l]] <- chart_prognose %>% arrange(desc(points)) %>% mutate(rank = 1:dim(.)[1] )
}


tb_cp_200000 <- map_dfr(cp_200000_no_update, bind_rows)


f_rank_prob <- function(dataset, team) {

  rank_prob <- map_df( .x = 1:16, ~ tibble(rank_prob = dataset %>%
                                             filter( rank == .x, teams == team ) %>%
                                             nrow()/filter(tb_cp_200000, teams == team) %>% nrow())) %>% 
    setNames(str_c(team))
  rank_prob
}
analysis <- map_df(unique(tb_cp_200000$teams), ~ f_rank_prob(dataset = tb_cp_200000, team = .x) ) %>%
  map_df( ~na.omit(.x)) %>%
  mutate(rank = 1:16) %>%
  select(rank, everything())

########simulation with draw

tie_prob_season <- database_season %>%
  filter(matchday == 20, season == 1920) %>%
  select(ties) %>%
  mutate(ties = as.numeric(ties))  %>%
  sum()/nrow(database_season %>% filter( between(as.numeric(matchday),1,20), season == 1920))

f_match_simulation <- function(rating , club_name_home, club_name_away, K, tie_prob_season = tie_prob_season ){
  
  
  rating_home <-  rating %>% filter(teams == club_name_home) %>% .$rating_value
  rating_away <-  rating %>% filter(teams == club_name_away) %>% .$rating_value
  
  dr_home <- rating_home - rating_away +100
  
  dr_away <- rating_away - rating_home
  
  W_e_home <- 1 / (10^(-dr_home/400) + 1)
  
  W_e_away <- 1 / (10^(-dr_away/400) + 1)
  
  
  tie_prob <- W_e_home * tie_prob_season
  

  W_team_home <- base::sample(x = c(1,0), size = 1, prob = c(W_e_home, W_e_away))
  
  W_team_away <- if (W_team_home == 1) 0 else 1
  
  rating$rating_value[rating$teams == club_name_home] <- rating_home + K * (W_team_home - W_e_home)
  
  rating$rating_value[rating$teams == club_name_away] <- rating_away + K * (W_team_away - W_e_away)
  rating
  
  
}
source(here::here("src/tie_prob.R"))

cp_200000_no_update <- list()
for (l in 1:200000){
  chart_prognose <- chart_prognose_reset
  
  for (j in unique(db_missing_games$matchday)){
    db_missing_matchday <- db_missing_games %>% filter( matchday == j)
    
    rating_sim <- rating_sim_reset
    for (i in seq_along(db_missing_matchday$matchday)) {
      
      rating_sim <- f_match_simulation_tie(rating = rating_sim,
                                       club_name_home = db_missing_matchday$club_name_home[i],
                                       club_name_away = db_missing_matchday$club_name_away[i],
                                       K = db_missing_matchday$K[i]
                                       
                                       
      )
    }
    
    for (m in seq_along(chart_prognose$teams)){
      chart_prognose$points[m] <- if (rating_sim$rating_value[m] > rating_sim$rating_old[m]) {chart_prognose$points[m] +3} else chart_prognose$points[m]
      
    }
  }
  cp_200000_no_update[[l]] <- chart_prognose %>% arrange(desc(points)) %>% mutate(rank = 1:dim(.)[1] )
}
