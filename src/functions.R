## functions for fussball.de vorhersage

# find_games_played(), finds all games that were played
# make_all_games(), creates list with all matchups
# sim_points(), calculates the winning probabilty of the home team using points


############################################################
## find_games_played(), finds all games that were played

find_games_played <- function(dbs_data, dbm_data){
  games_played <- data.frame(club_name_home = as.character(),
                             club_name_away = as.character(),
                             stringsAsFactors = FALSE)
  for(i in unique(dbs_data$club_name)){
    a <- dbm_data %>%
      filter(season == "1920", club_name_home == i) %>%
      select(club_name_away) %>%
      unique()
    b <- dbm_data %>%
      filter(season == "1920", club_name_away == i) %>%
      select(club_name_home) %>%
      unique()
    if(nrow(a) != 0){
      c <- data.frame(club_name_home = i,
                    club_name_away = a,
                    stringsAsFactors = FALSE)
    }
    if(nrow(b) != 0){
      d <- data.frame(club_name_home = b,
                      club_name_away = i,
                      stringsAsFactors = FALSE)
    }
    games_played <- bind_rows(games_played, c, d)
  }
  return(games_played)
}


############################################################
## make_all_games(), creates list with all matchups

make_all_games <- function(dbs_data, games_played){
  all_games <- games_played[0,]
  for(i in unique(dbs_data$club_name)){
    a <- data.frame(club_name_home = i,
                    club_name_away = unique(dbs_data$club_name),
                    stringsAsFactors = FALSE)
    c <- a[a$club_name_home != a$club_name_away,]
    all_games <- bind_rows(all_games, c)
  }
  return(all_games)
}




############################################################
# run_points_sim() runs N simulation runs for the missing games

run_points_sim <- function(missing_games, win_prob_home, sim_results, N){
  all_final_tables <- data.frame(stringsAsFactors = FALSE)
  for(i in 1:N){
    if(i %% 10 == 0){message("run ", i, " out of ", N)}
    rand_prob <- rbinom(nrow(missing_games), size = 1, p=win_prob_home)
    sim_results$score_home <- rand_prob*3
    sim_results$score_away <- (1-rand_prob)*3
    final_table <- data.frame(club_name = dbs1920$club_name, stringsAsFactors = FALSE)
    final_table$wins <- map_int(final_table$club_name, function(.x){
      nrow(filter(sim_results,
                  sim_results$club_name_away == .x,
                  sim_results$score_away != 0)) +
        nrow(filter(sim_results,
                    sim_results$club_name_home == .x,
                    sim_results$score_home != 0))
    })
    final_table$losses <- map_int(final_table$club_name, function(x){
      nrow(filter(sim_results,
                  sim_results$club_name_away == x,
                  sim_results$score_away == 0)) +
        nrow(filter(sim_results,
                    sim_results$club_name_home == x,
                    sim_results$score_home == 0))
    })
    final_table$score <- final_table$wins*3 
    all_final_tables <- bind_rows(all_final_tables, final_table)
  }
  return(all_final_tables)
}



############################################################
# sim_points(), calculates the winning probabilty of the home team using points

sim_points <- function(x,y, dbs_data){
  a <- dbs_data[dbs_data$club_name == x,]$points
  b <- dbs_data[dbs_data$club_name == y,]$points
  return(a/(a+b))
}
  
##### rating  for the first 20 matchdays (real data)

f_rating <- function(dataset , club_name_home, club_name_away){
  
  #Füge Tordifferenz hinzu
  rating <- tibble( teams = unique(club_name_home), rating_value = 1000)
  
  dataset<- dataset %>% drop_na() %>% mutate(goal_difference = pmax(goals_team_home, goals_team_away) - pmin(goals_team_home, goals_team_away) )
  
  
  
  
  
  dataset <- dataset %>%  mutate(K = case_when(goal_difference <= 1 ~ 30,
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

#### Simulate simple ratings for future matches 
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

###  creating score probabiblities
simulate_score_prob <- function(foot_model, homeTeam, awayTeam, max_goals=10){
  home_goals_avg <- predict(foot_model,
                            data.frame(home=1, team=homeTeam, 
                                       opponent=awayTeam), type="response")
  away_goals_avg <- predict(foot_model, 
                            data.frame(home=0, team=awayTeam, 
                                       opponent=homeTeam), type="response")
  prob_df <-  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
  map(1:nrow(prob_df), ~as.numeric(prob_df[.x,])) %>% unlist()
}


f_match_simulation_poisson <- function(rating , club_name_home, club_name_away, K, foot_model, max_goals = 10 ){
  
  
  rating_home <-  rating %>% filter(teams == club_name_home) %>% .$rating_value
  rating_away <-  rating %>% filter(teams == club_name_away) %>% .$rating_value
  
  dr_home <- rating_home - rating_away +100
  
  dr_away <- rating_away - rating_home
  
  W_e_home <- 1 / (10^(-dr_home/400) + 1)
  
  W_e_away <- 1 / (10^(-dr_away/400) + 1)
  
  
  
  score_prob <- simulate_score_prob(foot_model = foot_model, homeTeam = club_name_home, awayTeam = club_name_away, max_goals = max_goals)
  
  score_matrix <- map(0:max_goals, ~seq(from = .x*10, to = .x*10+max_goals )) %>% unlist() #%>%
    #set_names(~ map(0:max_goals, ~ str_c("goals", .x)) %>%
                #unlist() )  %>%
    #as_tibble(.name_repair = "unique") 
  
  W_team_home <- base::sample(x = score_matrix, size = 1, prob = score_prob)
  
  W_team_away <- if (W_team_home <= 10) 0 else if (W_team_home >= 10) W_team_home/10
  
  rating$rating_value[rating$teams == club_name_home] <- rating_home + K * (W_team_home - W_e_home)
  
  rating$rating_value[rating$teams == club_name_away] <- rating_away + K * (W_team_away - W_e_away)
  rating
  
  
}