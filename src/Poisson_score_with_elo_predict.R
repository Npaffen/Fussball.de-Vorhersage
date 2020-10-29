library(readr)
library(tidyverse)
database_mr <- read_rds(here::here( "/data/database_match_results_1819.rds"))%>% filter(between(matchday, 1, 20))

database_season <- read_rds(here::here("/data/database_season_1819.rds"))

missinggames <- read_rds(here::here("/data/database_match_results_1819.rds")) %>% filter(between(matchday, 21, max(database_season$matchday)))%>% .[1:4]



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
      goals=database_mr$goals_team_away,
      team=database_mr$club_name_away,
      opponent=database_mr$club_name_home,
      home=0)) %>%
  
  glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)
summary(poisson_model)


goals<- f_simulate_score_prob_poisson(foot_model = foot_model,
                                      homeTeam = missinggames$club_name_home,
                                      awayTeam = missinggames$club_name_away
)
missinggames <- missinggames %>% mutate(Goals_team_home = goals$home, Goals_team_away = goals$away)


if(database_season$season[1] == '1617'){
  matchday30 <- database_season %>% filter( matchday == 34) %>% filter(club_name != 'VfB Hüls II zg.')
  
}else {matchday30 <- database_season %>% filter( matchday == 30)}

final_table <- table_update(missinggames, matchday30)  
