library(readr)
library(tidyverse)
poisson_predict<- function(sim_years){
  database_mr <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))%>% filter(between(matchday, 1, 20))
  
  database_season <- read_rds(here::here(paste0("/data/database_season_",sim_years,".rds")))
  if(sim_years == 1920){
    missinggames <- read_rds(here::here(paste0("/data/database_missing_matches_1920.rds"))) %>% .[1:4]
    
  }else{
    missinggames <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds"))) %>% .[1:4]
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
  
  actual <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))
  tibble( Goals = 1:max(actual$goals_team_home),
    actual_home = map(1:max(actual$goals_team_home),~ filter(actual,goals_team_home == .x) %>%
                        nrow()/length(actual$club_name_home)) %>% unlist,
    actual_away =map(1:max(actual$goals_team_away),~ filter(actual,goals_team_away == .x) %>%
                       nrow()/length(actual$goals_team_away)) %>%
      unlist%>% append(),
    poisson_home =map(1:max(goals$home),~ filter(goals,home == .x) %>% nrow()/length(goals$home)) %>%
      unlist %>% append(rep(0,length(max(actual$goals_team_home)))),
    poisson_away =map(1:max(goals$away),~ filter(goals,away == .x) %>% nrow()/length(goals$away)) %>%
      unlist %>% append(rep(0,length(max(actual$goals_team_home))))
  
)
  
  home <- tibble( Goals = 1:max(actual$goals_team_home),
          actual_home = map(1:max(actual$goals_team_home),~ filter(actual,goals_team_home == .x) %>%
                              nrow()/length(actual$club_name_home)) %>% unlist)
          
  poisson_home <-map(1:max(goals$home),~ filter(goals,home == .x) %>% nrow()/length(goals$home)) %>%
            unlist %>% append(rep(0,length(max(actual$goals_team_home))))
  
  length(poisson_home) <- length(home$actual_home)
  
  transpose_df <- function(df) {
    t_df <- data.table::transpose(df)
    colnames(t_df) <- rownames(df)
    rownames(t_df) <- colnames(df)
    t_df <- t_df %>%
      tibble::rownames_to_column(.data = .) %>%
      tibble::as_tibble(.)
    return(t_df)
  }
  
  home <- home %>% mutate(poisson = poisson_home) %>% mutate_all(~replace(.,is.na(.),0))
  home <- transpose_df(home[2:3])
  
  home %>%
    gather( key=Type, value=Value) %>% 
    ggplot(aes(x=Value,fill=Type)) + 
    geom_histogram(position="dodge")+
    labs(x = 'Goals per Match' )
  
  tibble(poisson_away = goals$away,
         actual_away = actual$goals_team_away
  )%>% gather(key=Type, value=Value) %>% 
    ggplot(aes(x=Value,fill=Type)) + 
    geom_histogram(position="dodge")
}
sim_years <- c(1617,1718,1819,1920)