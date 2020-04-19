library(readr)
library(tidyverse)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results_1920.rds", sep = "/"))



database_season <- readRDS(here::here("/data/database_season.rds"))  
#Idee für Untenschieden : Ausrechen wie häufig unentschieden in dieser Saison gespielt wurde. 
#Annahme : Zwei gleichstarke Mannschaften haben eine höhere Wahrscheinlichkeit unentschieden zu spielen als zwei unterschiedlich Starke.
#Ableitung : Für gleichstarke Mannschaften Durchschnittswert für unterschiedlich Starke den Wert diskontieren 
#Proportion zum Rankingunterschied. 


f_rating_update <- function(rating , club_name_home, club_name_away, K, W_team_home, W_team_away ){
  
  
  rating_home <-  rating %>% filter(teams == club_name_home) %>% .$rating_value
  rating_away <-  rating %>% filter(teams == club_name_away) %>% .$rating_value
  
  dr_home <-rating_home - rating_away +100
  
  dr_away <-  rating_away - rating_home
  
  W_e_home <- 1 / (10^(-dr_home/400) + 1)
  
  W_e_away <- 1 / (10^(-dr_away/400) + 1)
  
  
  rating$rating_value[rating$teams == club_name_home] <- rating_home + K * (W_team_home - W_e_home)
  
  rating$rating_value[rating$teams == club_name_away] <- rating_away + K * (W_team_away - W_e_away)
  rating
}

f_match_simulation <- function(rating , club_name_home, club_name_away, K ){
  
  
  rating_home <-  rating %>% filter(teams == club_name_home) %>% .$rating_value
  rating_away <-  rating %>% filter(teams == club_name_away) %>% .$rating_value
  
  dr_home <- rating_home - rating_away +100
  
  dr_away <- rating_away - rating_home
  
  W_e_home <- 1 / (10^(-dr_home/400) + 1)
  
  W_e_away <- 1 / (10^(-dr_away/400) + 1)
  
  W_team_home <- base::sample(x = c(1,0), size = 1, prob = c(W_e_home, W_e_away))
  
  W_team_away <- if (W_team_home == 1) 0 else 1
  
  rating$rating_value[rating$teams == club_name_home] <- rating_home + K * (W_team_home - W_e_home)
  
  rating$rating_value[rating$teams == club_name_away] <- rating_away + K * (W_team_away - W_e_away)
  rating
  

}

#Füge Tordifferenz hinzu
database_mr <- database_mr %>% drop_na() %>% mutate(goal_difference = pmax(goals_team_home, goals_team_away) - pmin(goals_team_home, goals_team_away) )

rating <- tibble( teams = unique(database_mr$club_name_home), rating_value = 1000)



database_mr <-database_mr %>%  mutate(K = case_when(goal_difference <= 1 ~ 30,
                                                    goal_difference == 2 ~ 30+0.5,
                                                    goal_difference == 3 ~ 30+3/4,
                                                    goal_difference >= 4 ~ 30+ 3/4+ (goal_difference -3) / 8,
                                                    TRUE ~ 1),
                                      W_team_home = case_when( pmax(goals_team_home, goals_team_away) == goals_team_home ~ 1,
                                                               goal_difference == 0 ~ 0.5,
                                                               TRUE ~ 0),
                                      W_team_away = case_when(pmax(goals_team_home, goals_team_away) == goals_team_away ~ 1,
                                                              goal_difference == 0 ~ 0.5,
                                                              TRUE ~ 0)
                                        
)# K ist der Gewichtungsfaktor pro Spiel pro Tordifferenz


  







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
rating_sim <- rating %>% select(teams, rating_value) %>% mutate(rating_old = rating_value)
#Tabelle um Gewinne zu zählen
chart_prognose_reset <-  tibble(teams = rating_sim$teams) %>% inner_join( points_last_md)

cp_20000 <- list()
rating_sim_2000 <- list()
for (l in 1:20000){

  for (j in unique(db_missing_games$matchday)){
db_missing_matchday <- db_missing_games %>% filter( matchday == j)
rating_sim$rating_old <- rating_sim$rating_value
chart_prognose <- chart_prognose_reset
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



  
