## functions for fussball.de vorhersage

# find_games_played(), finds all games that were played
# make_all_games(), creates list with all matchups
# sim_points(), calculates the winning probabilty of the home team using points


############################################################
## find_games_played(), finds all games that were played

find_games_played <- function(dbs_data, dbm_data){
  games_played <- data.frame(club_name_home = as.character(),
                             club_name_away = as.character())
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
                    club_name_away = a)
    }
    if(nrow(b) != 0){
      d <- data.frame(club_name_home = b,
                      club_name_away = i)
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
                    club_name_away = unique(dbs_data$club_name))
    b <- data.frame(club_name_home = unique(dbs_data$club_name),
                    club_name_away = i)
    c <- bind_rows(a, b) %>%
      filter(club_name_home != club_name_away)
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
  
