library(readr)
library(tidyverse)
poisson_plot<- function(sim_years){
  database_mr <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))%>% filter(between(matchday, 1, 20))
  
  database_season <- read_rds(here::here(paste0("/data/database_season_",sim_years,".rds")))
  
  missinggames <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds"))) %>% .[1:4]
  
  
  
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
  
  
  goals<- f_simulate_score_prob_poisson(foot_model = poisson_model,
                                        homeTeam = missinggames$club_name_home,
                                        awayTeam = missinggames$club_name_away
  )
  actual <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))

  png(here::here(paste0("paper/plots/poisson_actual_home_",sim_years,".png")))
 home <- tibble(poisson_away = goals$home,
         actual_away = actual$goals_team_home
  )%>% gather(key=Type, value=Value) %>% 
    ggplot(aes(x=Value,fill=Type)) + 
    geom_histogram(position="dodge")+
    labs(x = 'Goals per Match' )+
   labs(x = 'Goals per Match', y = 'Frequency of Goals' )
 home + scale_x_continuous(breaks = 0:15)+
   scale_y_continuous(breaks = seq(0,100,5))
 dev.off()
 
 png(here::here(paste0("paper/plots/poisson_actual_away_",sim_years,".png")))
 away <- tibble(poisson_away = goals$away,
         actual_away = actual$goals_team_away
  )%>% gather(key=Type, value=Value) %>% 
    ggplot(aes(x=Value,fill=Type)) + 
    geom_histogram(position="dodge")+
    labs(x = 'Goals per Match', y = 'Frequency of Goals' )
away + scale_x_continuous(breaks = 0:10)+
  scale_y_continuous(breaks = seq(0,100,5))
dev.off()
 }
sim_years <- c(1617,1718,1819)
map(sim_years, ~poisson_plot(.x))
