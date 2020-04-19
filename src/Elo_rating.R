library(readr)
library(tidyverse)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results_1920.rds", sep = "/"))



database_season <- readRDS(here::here("/data/database_season.rds"))  
#Idee für Untenschieden : Ausrechen wie häufig unentschieden in dieser Saison gespielt wurde. 
#Annahme : Zwei gleichstarke Mannschaften haben eine höhere Wahrscheinlichkeit unentschieden zu spielen als zwei unterschiedlich Starke.
#Ableitung : Für gleichstarke Mannschaften Durchschnittswert für unterschiedlich Starke den Wert diskontieren 
#Proportion zum Rankingunterschied. 

#Füge Tordifferenz hinzu
database_mr <- database_mr %>% drop_na() %>% mutate(goal_difference = pmax(goals_team_home, goals_team_away) - pmin(goals_team_home, goals_team_away) )

rating <- tibble( teams = unique(database_mr$club_name_home), rating = 1000)



database_mr <-database_mr %>%  mutate(K = case_when(goal_difference <= 1 ~ 30,
                                                    goal_difference == 2 ~ 30+30*0.5,
                                                    goal_difference == 3 ~ 30+30*3/4,
                                                    goal_difference >= 4 ~ 30+ 30*(3/4+ (goal_difference -3) / 8),
                                                    TRUE ~ 1),
                                      W_team_home = case_when( pmax(goals_team_home, goals_team_away) == goals_team_home ~ 1,
                                                               goal_difference == 0 ~ 0.5,
                                                               TRUE ~ 0),
                                      W_team_away = case_when(pmax(goals_team_home, goals_team_away) == goals_team_away ~ 1,
                                                              goal_difference == 0 ~ 0.5,
                                                              TRUE ~ 0)
                                        
)# K ist der Gewichtungsfaktor pro Spiel pro Tordifferenz


  



f_rating_update <- function(rating , club_name_home, club_name_away, K, W_team_home, W_team_away ){

 
rating_home <-  rating %>% filter(teams == club_name_home) %>% .$rating
rating_away <-  rating %>% filter(teams == club_name_away) %>% .$rating

dr_home <-rating_home - rating_away +100

dr_away <-  rating_away - rating_home

#Hier muss noch W_team_home kalkuliert werden

W_team_away

W_e_home <- 1 / (10^(-dr_home/400) + 1)

W_e_away <- 1 / (10^(-dr_away/400) + 1)


rating$rating[rating$teams == club_name_home] <- rating_home + K * (W_team_home - W_e_home)

rating$rating[rating$teams == club_name_away] <- rating_away + K * (W_team_away - W_e_away)
rating
}



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
rating <- rating %>% arrange(desc(rating))
db_missing_games <- read_rds(here::here("/data/database_missing_matches_1920.rds"))



# - check games status


