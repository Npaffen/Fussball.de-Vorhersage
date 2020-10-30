library(readr)
library(tidyverse)
poisson_predict<- function(sim_years){
database_mr <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))%>% filter(between(matchday, 1, 20))

database_season <- read_rds(here::here(paste0("/data/database_season_",sim_years,".rds")))
if(sim_years == 1920){
  missinggames <- read_rds(here::here(paste0("/data/database_missing_matches_1920.rds"))) %>% filter(between(matchday, 21, max(database_season$matchday)))%>% .[1:4]
  
}else{
missinggames <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds"))) %>% filter(between(matchday, 21, max(database_season$matchday)))%>% .[1:4]
}


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
missinggames <- missinggames %>% mutate(Goals_team_home = goals$home, Goals_team_away = goals$away)


if(database_season$season[1] == '1617'){
  matchday30 <- database_season %>% filter( matchday == 34) %>% filter(club_name != 'VfB Hüls II zg.')
  
}else {matchday30 <- database_season %>% filter( matchday == 30)}

final_table <- table_update(missinggames, matchday30)  

saveRDS(final_table, paste0(getwd(), "/data/poisson_score_simulation_",sim_years,"_predict.rds"))
}
sim_years <- c(1617,1718,1819,1920)

map(.x = sim_years,~poisson_predict(sim_years = .x))

library(ggplot2)
actual <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))%>%
  filter(between(matchday, 21, max(database_season$matchday))) 
poisson <- missinggames 


tibble(home = actual$goals_team_home, away = actual$goals_team_away) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=Value,fill=Type)) + 
  geom_histogram(position="dodge")
  #geom_line(data=poisson, aes(y=density , x = Goals_team_home), colour="green")
  #geom_line(data=dnorm(poisson$Goals_team_away), aes(y=density), colour="blue")
  
  #tibble(home = poisson$Goals_team_home, away = poisson$Goals_team_away) %>% 
  #gather(key=Type, value=Value) %>%
  #geom_density(aes(x=Value,fill=Type))
  
  #geom_density(poisson$Goals_team_home)

  print(p0)
  
  data <- rbind(data.frame(type="home", value=actual$goals_team_home, stringsAsFactors=FALSE),
                data.frame(type="away", value=actual$goals_team_home, stringsAsFactors=FALSE))
  
  estimate <- group_by(data, type) %>% summarize(mu=mean(value))
  dens <- expand.grid(value=0:max(data$value), type=c("away", "home"),
                      stringsAsFactors=FALSE) %>%
    inner_join(estimate) %>%
    mutate(density=dpois(value, mu))
  prop <- group_by(data, type, value) %>% summarize(count=n()) %>%
    group_by(type) %>% mutate(prop=count/sum(count)) 
  tmp_actual <- left_join(dens, prop) %>% replace_na(list(prop=0, count=0))
  
  data <- rbind(data.frame(type="home", value=poisson$Goals_team_home, stringsAsFactors=FALSE),
                data.frame(type="away", value=poisson$Goals_team_home, stringsAsFactors=FALSE))
  
  estimate <- group_by(data, type) %>% summarize(mu=mean(value))
  dens <- expand.grid(value=0:max(data$value), type=c("away", "home"),
                      stringsAsFactors=FALSE) %>%
    inner_join(estimate) %>%
    mutate(density=dpois(value, mu))
  prop <- group_by(data, type, value) %>% summarize(count=n()) %>%
    group_by(type) %>% mutate(prop=count/sum(count)) 
  tmp_poisson <- left_join(dens, prop) %>% replace_na(list(prop=0, count=0))%>%
    add_row(value = 7,type = 'home',mu = dens$mu[1],density = 0,count = 0, prop =0) %>%
    add_row(value = 7,type = 'away',mu = dens$mu[1],density = 0,count = 0, prop =0) %>%
    add_row(value = 8,type = 'home',mu = dens$mu[1],density = 0,count = 0, prop =0) %>%
    add_row(value = 88,type = 'away',mu = dens$mu[1],density = 0,count = 0, prop =0) %>%
  
  ggplot(tmp_actual, aes(x=value, weight=prop, fill=type)) + 
    geom_bar(position="dodge") +
    geom_line(aes(tmp_poisson$value, tmp_poisson$density, color=type, weight=NULL))
