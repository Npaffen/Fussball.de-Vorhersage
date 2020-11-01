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
  
  
 sim <- f_simulate_score_prob(foot_model = poisson_model,
                                        homeTeam = missinggames$club_name_home,
                                        awayTeam = missinggames$club_name_away
  ) 
poisson <- tibble(all_goals = c(sim$home, sim$away))  
return(poisson)
 }


sim_years <- c(1617,1718,1819)
poisson_method <- map_df(sim_years, ~poisson_plot(.x)) 



actual <- rbind(read_rds(here::here(paste0("/data/database_match_results_",sim_years[1],".rds"))) %>% .[,5:6],
                read_rds(here::here(paste0("/data/database_match_results_",sim_years[2],".rds"))) %>% .[,5:6],
                read_rds(here::here(paste0("/data/database_match_results_",sim_years[3],".rds"))) %>% .[,5:6])

actual <- tibble(all_goals = c(actual$goals_team_home, actual$goals_team_away))
<<<<<<< Updated upstream
png(file = here::here(paste0("paper/plots/poisson_actual_home.png")) )
poisson2 <- tibble(poisson = poisson_method$all_goals,
=======
png(file = here::here(paste0("paper/plots/poisson_actual.png")) )
poisson <- tibble(poisson = poisson_method$all_goals,
>>>>>>> Stashed changes
               actual = actual$all_goals
)%>% gather(key=Type, value=Value) %>% 
  ggplot(aes(x=Value,fill=Type)) + 
  geom_histogram(position="dodge")+
  labs(x = 'Goals per Match' )+
  labs(x = 'Goals per Match', y = 'Frequency of Goals' )+ 
  scale_x_continuous(breaks = 0:20)+
  scale_y_continuous(breaks = seq(0,800,50))
print(poisson2)
dev.off()
