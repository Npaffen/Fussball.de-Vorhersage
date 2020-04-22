library(readr)
library(tidyverse)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results_1920.rds", sep = "/"))

missinggames <- read_rds(here::here("/data/database_missing_matches_1920.rds"))

database_season <- readRDS(here::here("/data/database_season_1920.rds"))

source(here::here("src/functions_N.R"))
#Idee für Untenschieden : Ausrechen wie häufig unentschieden in dieser Saison gespielt wurde. 
#Annahme : Zwei gleichstarke Mannschaften haben eine höhere Wahrscheinlichkeit unentschieden zu spielen als zwei unterschiedlich Starke.
#Ableitung : Für gleichstarke Mannschaften Durchschnittswert für unterschiedlich Starke den Wert diskontieren 
#Proportion zum Rankingunterschied. 









function(played_matchdays , club_name_home, club_name_away)




played_matchdays <- database_mr %>% filter(between(matchday,1,20))


#create ratings for the first 20 matchdays

  rating <- f_rating(played_matchdays)

matchday30 <- database_season %>% filter(matchday == 30)

N = 5000
f_rating_prob_matches( missinggames, matchday30, rating, ties = T, N, limit = 0.000001 )
