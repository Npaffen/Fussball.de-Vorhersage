f_match_simulation_tie <- function(rating , club_name_home, club_name_away, K ){
  
  # rating$rating_value <- rating$rating_value + rnorm(16,0,50)
  #club_name_home <- "1. SC BW Wulfen"
  #club_name_away <- "FC RW Dorsten"
  
  rating_home <-  rating %>% filter(teams == club_name_home) %>% .$rating_value
  rating_away <-  rating %>% filter(teams == club_name_away) %>% .$rating_value
  
  rating_home <- 2000
  
  dr <- rating_home - rating_away +100
  
  tie_prob <- 0.12 # empirical tie probability
  
  W_e_home <- 1 / (10^(+dr/400) + 1) - tie_prob/2
  W_e_away <- 1 / (10^(-dr/400) + 1) - tie_prob/2
  
  if(W_e_home <= 0){
    tie_prob <- tie_prob + W_e_home
    W_e_home <- 0
  }
  
  if(W_e_away <= 0){
    tie_prob <- tie_prob + W_e_away
    W_e_away <- 0
  }
  
  W_e_away + W_e_home + tie_prob
  
  W_team_home <- base::sample(x = c(1,0), size = 1, prob = c(W_e_home, W_e_away))
  
  W_team_away <- if (W_team_home == 1) 0 else 1
  
  rating$rating_value[rating$teams == club_name_home] <- rating_home + K * (W_team_home - W_e_home)
  
  rating$rating_value[rating$teams == club_name_away] <- rating_away + K * (W_team_away - W_e_away)
  rating
  
  
}
