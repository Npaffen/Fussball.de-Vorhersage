# !diagnostics suppress=<played_matchdays>

##### rating  for the first 20 matchdays (real data)

f_rating <- function(played_matchdays){
  
  #Füge Tordifferenz hinzu
  rating <- tibble( teams = unique(played_matchdays$club_name_home), rating_value_home = 1000, rating_value_away = 1000)
  
  played_matchdays<- played_matchdays %>%
    mutate(goal_difference = pmax(goals_team_home, goals_team_away) - pmin(goals_team_home, goals_team_away) )
  
  
  
  
  
  played_matchdays <- played_matchdays %>%  mutate(K = case_when(goal_difference <= 1 ~ 30,
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
  rating_home <- as.numeric()
  rating_away <- as.numeric()
  dr_home <- as.numeric()
  W_e_home <- as.numeric()
  W_e_away <- as.numeric()
  for (m in seq_along(played_matchdays$club_name_home)){
  rating_home <-  rating %>% filter(teams == played_matchdays$club_name_home[m]) %>% .$rating_value_home
  rating_away <-  rating %>% filter(teams == played_matchdays$club_name_away[m]) %>% .$rating_value_away
  
  dr_home <-rating_home - rating_away +100
  
  dr_away <-  1 - dr_home

  W_e_home <- 1 / (10^(-dr_home/400) + 1)
  
  W_e_away <- 1 / (10^(-dr_away/400) + 1)
  
  
  rating$rating_value_home[rating$teams == played_matchdays$club_name_home[m]] <- rating_home + played_matchdays$K[m] * (played_matchdays$W_team_home[m] - W_e_home)
  
  rating$rating_value_away[rating$teams == played_matchdays$club_name_away[m]] <- rating_away + played_matchdays$K[m] * (played_matchdays$W_team_away[m] - W_e_away)
  }
  rating
}


###  creating score probabiblities
f_simulate_score_prob <- function(foot_model, homeTeam, awayTeam){
  
  
  home_goals_avg <- map2_df(.x = homeTeam, .y = awayTeam,~predict(foot_model,
                            data.frame(home=1, team=.x, 
                                       opponent=.y), type="response") %>% floor
  )
  away_goals_avg <- map2_df(.x = homeTeam, .y = awayTeam,~predict(foot_model, 
                            data.frame(home=0, team=.y, 
                                       opponent=.x), type="response") %>% floor
  )
  goals <- data.frame(home = home_goals_avg$`1`, away = away_goals_avg$`1`)
  return(goals)
  #map(1:nrow(prob_df), ~as.numeric(prob_df[.x,])) %>% unlist()
}



table_update <- function(missinggames, matchday30){
  clubs <- matchday30$club_name
  missinggames <- missinggames %>%
    mutate(points_team_home = case_when(Goals_team_home > Goals_team_away ~3, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 0),
           points_team_away = case_when(Goals_team_home > Goals_team_away ~0, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 3),
           Goal_diff_home = Goals_team_home - Goals_team_away,
           Goal_diff_away = Goals_team_away - Goals_team_home
           )
  
  points_update <- map(clubs, ~ missinggames %>%
                         filter(club_name_home == .x ) %>%
                         dplyr::select(points_team_home) %>%
                         sum()+ missinggames %>% 
                         filter(club_name_away == .x ) %>%
                         dplyr::select(points_team_away) %>% 
                         sum()) %>%
    unlist 
  
  
  
  goal_diff_update <- map(clubs, ~ missinggames %>%
                            filter(club_name_home == .x ) %>%
                            dplyr::select(Goal_diff_home) %>%
                            sum()+ missinggames %>% 
                            filter(club_name_away == .x ) %>%
                            dplyr::select(Goal_diff_away) %>% 
                            sum()) %>%
    unlist 
  #all_scores <- all_scores %>%
  # inner_join(missinggames %>% dplyr::select(Goals_team_away, Goals_team_away)
  # )
  
  final_table <- matchday30 %>%
    mutate( points_new =  points_update,
            points = points + points_new,
            goal_diff_new = goal_diff_update,
            goal_diff = goal_diff + goal_diff_new) %>%
    dplyr::select(club_name, points, goal_diff) %>%
    arrange(desc(points), desc(goal_diff)) %>%
    mutate(rank = 1:length(club_name)) %>%
    dplyr::select(rank, everything())
  
  return(final_table)
  
}




#simulate poisson score prob model
f_score_prob_matches <- function( missinggames, matchday30, foot_model, max_goals = 10, N, limit ){
  all_avg_tables <- data.frame(stringsAsFactors = FALSE)
  all_final_tables <- data.frame(stringsAsFactors = FALSE)
  clubs <- matchday30$club_name
  matchday30_reset <- matchday30
  conv <- numeric()
  all_scores <-tibble(Goals_team_home = 0, Goals_team_away = 0)
  for(i in 1:N){
    Goals_team_away_u <- 0
    Goals_team_home_u <- 0
    matchday30 <- matchday30_reset
      for (m in seq_along(missinggames$club_name_home)){
        score_prob <- f_simulate_score_prob(foot_model = foot_model,
                                          homeTeam = missinggames$club_name_home[m],
                                          awayTeam = missinggames$club_name_away[m],
                                          max_goals = max_goals)
        
        score <- base::sample(x = score_prob, size = 1, prob = score_prob)
        
        Goals_team_home_u[m] <- which(score_prob == score, arr.ind = T)[1]
        
        Goals_team_away_u[m] <- which(score_prob == score, arr.ind = T)[2]
        }
    
    missinggames <- missinggames %>% mutate(Goals_team_home = Goals_team_home_u,
                                            Goals_team_away = Goals_team_away_u,
                                            Goal_diff_home = Goals_team_home_u - Goals_team_away_u,
                                            Goal_diff_away = Goals_team_away_u - Goals_team_home_u)
    
    missinggames <- missinggames %>%
      mutate(points_team_home = case_when(Goals_team_home > Goals_team_away ~3, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 0),
             points_team_away = case_when(Goals_team_home > Goals_team_away ~0, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 3))
    
    
    points_update <- map(clubs, ~ missinggames %>%
                          filter(club_name_home == .x ) %>%
                          dplyr::select(points_team_home) %>%
                          sum()+ missinggames %>% 
                          filter(club_name_away == .x ) %>%
                          dplyr::select(points_team_away) %>% 
                          sum()) %>%
      unlist 
    
       
    
    goal_diff_update <- map(clubs, ~ missinggames %>%
                             filter(club_name_home == .x ) %>%
                             dplyr::select(Goal_diff_home) %>%
                             sum()+ missinggames %>% 
                             filter(club_name_away == .x ) %>%
                             dplyr::select(Goal_diff_away) %>% 
                             sum()) %>%
      unlist 
    #all_scores <- all_scores %>%
     # inner_join(missinggames %>% dplyr::select(Goals_team_away, Goals_team_away)
   # )
    
    final_table <- matchday30 %>%
      mutate( points_new =  points_update,
              points = points + points_new,
              goal_diff_new = goal_diff_update,
              goal_diff = goal_diff + goal_diff_new) %>%
      dplyr::select(club_name, points, goal_diff) %>%
      arrange(desc(points), desc(goal_diff)) 
    
    all_final_tables <- bind_rows(all_final_tables,final_table)
    
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
    conv_speed <- sum(abs(average_table$points-average_table2$points))
    conv <- c(conv, conv_speed)
    if(i %% 10 == 0){
      message("convergence speed: ", round(conv_speed, 3), " run ", i, " out of ", N)
    }
    if(conv_speed < limit){
      message("converged!")
      break
    }
  }
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
                     "all_avg_tables" = all_avg_tables#,
                     #"all_scores" = all_scores
  )
  return(sim_output)
}
#simulation of elo rating model
f_rating_prob_matches <- function( missinggames, matchday30, rating, ties, N, limit, season ){
  all_avg_tables <- data.frame(stringsAsFactors = FALSE)
  all_final_tables <- data.frame(stringsAsFactors = FALSE)
  
  clubs <- matchday30$club_name
  
  matchday30_reset <- matchday30
  
  tie_prob <- database_season %>%
    filter( season == season) %>%
    dplyr::select(ties) %>% 
    sum/length(database_season$ties)
  
  
  conv <- numeric()
  for(i in 1:N){
    Goals_team_away_u <- 0
    Goals_team_home_u <- 0
    matchday30 <- matchday30_reset
    rating_home <- as.numeric()
    rating_away <- as.numeric()
    dr_home <- as.numeric()
    W_e_home <- as.numeric()
    W_e_away <- as.numeric()
    W_team_home <- as.numeric()
    W_team_away <- as.numeric()
    for (m in seq_along(missinggames$club_name_home)){
      rating_home <-  rating %>% filter(teams == missinggames$club_name_home[m]) %>% .$rating_value_home
      rating_away <-  rating %>% filter(teams == missinggames$club_name_away[m]) %>% .$rating_value_away
      
      
      dr_home <- rating_home - rating_away +100
      
      dr_away <-  rating_away - rating_home 
      
      if (ties == T){
        
        W_e_home <- 1 / (10^(-dr_home/400) + 1) - tie_prob/2
        W_e_away <- 1 / (10^(-dr_away/400) + 1) - tie_prob/2
      } else {
        
        W_e_home <- 1 / (10^(-dr_home/400) + 1)
        W_e_away <- 1 / (10^(-dr_away/400) + 1)
      }
      
      if(W_e_home <= 0){
        tie_prob <- tie_prob + W_e_home
        W_e_home <- 0
      }
      
      if(W_e_away <= 0){
        tie_prob <- tie_prob + W_e_away
        W_e_away <- 0
      }
      
      # W_e_away + W_e_home + tie_prob
      
      W_team_home[m] <- base::sample(x = c(3,0,1), size = 1, prob = c(W_e_home, W_e_away,tie_prob))
      
      W_team_away[m] <- if (W_team_home[m] == 3) 0 else if (W_team_home[m] == 1) 1 else 3
      
    }
    
    missinggames <- missinggames %>% mutate(points_team_home = W_team_home,
                                            points_team_away = W_team_away,
    )
    
    
    points_update <- map(clubs, ~ missinggames %>%
                           filter(club_name_home == .x ) %>%
                           dplyr::select(points_team_home) %>%
                           sum()+ missinggames %>% 
                           filter(club_name_away == .x ) %>%
                           dplyr::select(points_team_away) %>% 
                           sum()) %>%
      unlist 
    
    
    final_table <- matchday30 %>%
      mutate( points_new =  points_update,
              points = points + points_new)%>%
      dplyr::select(club_name, points) %>%
      arrange(desc(points)) 
    
    all_final_tables <- bind_rows(all_final_tables,final_table)
    
    average_table <- aggregate(
      all_final_tables[1:(length(clubs)*(i-1)),],
      by = list(all_final_tables$club_name[1:(length(clubs)*(i-1))]),
      FUN = "mean"
    )
    average_table2 <- aggregate(
      all_final_tables,
      by = list(all_final_tables$club_name),
      FUN = "mean"
    )
    all_avg_tables <- bind_rows(all_avg_tables, average_table2)
    conv_speed <- sum(abs(average_table$points-average_table2$points))
    conv <- c(conv, conv_speed)
    if(i %% 10 == 0){
      message("convergence speed: ", round(conv_speed, 3), " run ", i, " out of ", N)
    }
    if(conv_speed < limit){
      message("converged!")
      break
    }
  }
  if(conv_speed < limit){
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("converged to below ", limit, 
                                     " after ", length(conv), " runs"))
  } else {
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("didn't converge below ", limit, 
                                     " after ", N, " runs"))
  }
  all_avg_tables <- all_avg_tables %>%  dplyr::select(Group.1 , points) %>% rename(club_name = Group.1, score = points)
  
  sim_output <- list("all_final_tables" = all_final_tables,
                     "conv_plot" = conv_plot,
                     "all_avg_tables" = all_avg_tables
  )
  return(sim_output)
}

############################################################
## add_rank_col(), adds run and rank column to all_tables output

add_run_rank_col <- function(x = all_final_tables){
  rank_count <- length(unique(x$club_name))
  x$run <- rep(1:(length(x$score)/rank_count), each = rank_count)
  x$rank <- NA
  for(i in 1:(length(x$score)/rank_count)){
    tab <- x[x$run == i,]
    tab <- transform(tab, rank = rank(-score, ties.method = "first"))
    x[x$run == i,] <- tab
  }
  return(x)
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




