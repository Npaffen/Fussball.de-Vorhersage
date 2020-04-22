library(readr)
library(tidyverse)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results_1920.rds", sep = "/")) 


database_season <- readRDS(here::here("/data/database_season.rds"))  

source(here::here("src/functions_N.R"))
#Idee für Untenschieden : Ausrechen wie häufig unentschieden in dieser Saison gespielt wurde. 
#Annahme : Zwei gleichstarke Mannschaften haben eine höhere Wahrscheinlichkeit unentschieden zu spielen als zwei unterschiedlich Starke.
#Ableitung : Für gleichstarke Mannschaften Durchschnittswert für unterschiedlich Starke den Wert diskontieren 
#Proportion zum Rankingunterschied. 



home_goals_avg <- database_mr$goals_team_home %>% mean()

away_goals_avg <- database_mr$goals_team_away %>% mean()

poisson_model <- 
  bind_rows(
    tibble(
      goals = database_mr$goals_team_home,
      team = database_mr$club_name_home,
      opponent=database_mr$club_name_away,
      home=1),
    tibble(
      goals=database_mr$goals_team_home,
      team=database_mr$club_name_home,
      opponent=database_mr$club_name_away,
      home=0)) %>%
  
  glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)
summary(poisson_model)



simulate_match(poisson_model, "Chelsea", "Sunderland", max_goals=4)

N <- 5000 # simulation runs
sim_output <- f_score_prob_matches(missinggames, matchday30, poisson_model, N, limit = 0.01)
all_final_tables <- sim_output[[1]]
sim_output[[2]] # print convergence plot
average_table <- aggregate(all_final_tables[,-1],
                           by = list(all_final_tables$club_name),
                           FUN = "mean")
