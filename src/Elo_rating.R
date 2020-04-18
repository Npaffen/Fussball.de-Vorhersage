library(readr)
library(tidyverse)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results_1920.rds", sep = "/"))



database_season <- readRDS(here::here("/data/database_season.rds"))  

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



matchday_results <- filter(database_mr, matchday == 1)
f_rating_update <- function(rating , matchday)
rating_home <-  rating %>% filter(teams == matchday_results$club_name_home) %>% .$rating
rating_away <-  rating %>% filter(teams == matchday_results$club_name_away) %>% .$rating

dr_home <-rating_home - rating_away +100

dr_away <-  rating_away - rating_home


W_e_home <- 1 / (10^(-dr_home/400) + 1)

W_e_away <- 1 / (10^(-dr_away/400) + 1)


rating$rating[rating$teams == matchday_results$club_name_home] <- rating_home + matchday_results$K * (matchday_results$W_team_home - W_e_home)

R_n_home

rating$rating[rating$teams == matchday_results$club_name_away] <- rating_away + matchday_results$K * (matchday_results$W_team_away - W_e_away)



