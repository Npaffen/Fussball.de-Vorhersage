## functions for fussball.de vorhersage

require(dplyr)
require(purrr)
require(ggplot2)

# add_run_rank_rank_col(), adds run and rank column to all_tables output
# find_games_played(), finds all games that were played
# make_all_games(), creates list with all matchups
# make_plot(), makes output plots
# sim_points(), calculates the winning probabilty of the home team using points


############################################################
## add_rank_col(), adds run and rank column to all_tables output

add_run_rank_col <- function(x = all_final_tables){
  x$run <- rep(1:(length(x$score)/16), each = 16)
  x$rank <- NA
  for(i in 1:(length(x$score)/16)){
    tab <- x[x$run == i,]
    tab <- transform(tab, rank = rank(-score, ties.method = "first"))
    x[x$run == i,] <- tab
  }
  return(x)
}

############################################################
## find_games_played(), finds all games that were played

find_games_played <- function(dbs_data, dbm_data, season){
  games_played <- data.frame(club_name_home = as.character(),
                             club_name_away = as.character(),
                             stringsAsFactors = FALSE)
  for(i in unique(dbs_data$club_name)){
    a <- dbm_data %>%
      filter(season == season, club_name_home == i) %>%
      select(club_name_away) %>%
      unique()
    b <- dbm_data %>%
      filter(season == season, club_name_away == i) %>%
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
## make_plot(), makes output plots

make_plot <- function(x = all_final_tables,
                      y = "rank",
                      club_names = c("SV Altendorf-Ulfkotte",
                                     "SV Schermbeck II",
                                     "TuS Gahlen",
                                     "VfL Ramsdorf"),
                      type = "hist"){
  dfa <- x
  dfa <- filter(dfa, club_name %in% club_names)
  if(type == "line"){
    make_plot <- ggplot(dfa, aes_string(x="run", y=y, colour = "club_name"))
    return(make_plot + geom_line())
  }
  if(type == "hist"){
    make_plot <- ggplot(dfa, aes_string(x=y, fill="club_name"))
    return(make_plot + 
             geom_bar(aes(y = (..count..)/sum(..count..))) + 
             scale_y_continuous(labels = scales::percent)+
             labs(title= paste0(y, " histogram")) 
    )
  }
}




############################################################
# run_points_sim() runs N simulation runs for the missing games

run_points_sim <- function(missing_games, dbs1920, 
                           win_prob_home, sim_results, N, limit){
  old_table <- data.frame(club_name = dbs1920$club_name,
                          wins = dbs1920$wins,
                          losses = dbs1920$loss,
                          score = dbs1920$points,
                          stringsAsFactors = FALSE)
  # simulated games
  sim_results <- missing_games 
  # winning probabilites based on points
  win_prob_home <- map2(
    sim_results$club_name_home,
    sim_results$club_name_away,
    ~sim_points(.x, .y, dbs_data = dbs1920)
  )
  win_prob_home <- unlist(win_prob_home)
  # simulated final season table
  all_final_tables <- data.frame(stringsAsFactors = FALSE)
  all_avg_tables <- data.frame(stringsAsFactors = FALSE)
  clubs <- unique(missing_games$club_name_home)
  # track convergence speed
  conv <- numeric()
  # run simulation
  for(i in 1:N){
    draw_result <- rbinom(nrow(missing_games), size = 1, p=win_prob_home)
    sim_results$score_home <- draw_result*3
    sim_results$score_away <- (1-draw_result)*3
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
    # sort final_table and add results from games_played
    final_table <- final_table[order(final_table$club_name),]
    old_table <- old_table[order(old_table$club_name),]
    final_table[,2:4] <- final_table[,2:4]+old_table[,2:4]
    # record run results
    all_final_tables <- bind_rows(all_final_tables, final_table)
    # calculate average of all runs so far to calculate conv speed
    average_table <- aggregate(
      all_final_tables[1:(length(clubs)*(i-1)),-1],
      by = list(all_final_tables$club_name[1:(length(clubs)*(i-1))]),
      FUN = "mean"
    )
    average_table2 <- aggregate(
      all_final_tables[,-1],
      by = list(all_final_tables$club_name),
      FUN = "mean"
    )
    all_avg_tables <- bind_rows(all_avg_tables, average_table2)
    conv_speed <- sum(abs(average_table$score-average_table2$score))
    conv <- c(conv, conv_speed)
    # report speed and progress
    if(i %% 10 == 0){
      message("convergence speed: ", round(conv_speed, 3), " run ", i, " out of ", N)
    }
    if(conv_speed < limit){
      message("converged!")
      break
    }
  }
  # save convergence plot
  if(conv_speed < limit){
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("converged to below ", limit, 
                                     " after ", length(conv), " runs"))
  } else {
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("didn't converge below ", limit, 
                                     " after ", N, " runs"))
  }
  all_avg_tables <- rename(all_avg_tables, club_name = Group.1)
  sim_output <- list("all_final_tables" = all_final_tables,
                     "conv_plot" = conv_plot,
                     "sim_results" = sim_results,
                     "win_prob_home" = cbind(missing_games,win_prob_home=win_prob_home),
                     "all_avg_tables" = all_avg_tables
                     )
  return(sim_output)
}



############################################################
# sim_points(), calculates the winning probabilty of the home team using points

sim_points <- function(x,y, dbs_data){
  a <- dbs_data[dbs_data$club_name == x,]$points
  b <- dbs_data[dbs_data$club_name == y,]$points
  return(a/(a+b))
}
  