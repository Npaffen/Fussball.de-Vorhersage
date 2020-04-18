library(readr)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results.rds", sep = "/"))



database_season <- readRDS(here::here("/data/database_season.rds"))  

#Füge Torunterschiede hinzu
database_mr <- database_mr %>% mutate(goal_difference = pmax(goals_team_home, goals_team_away) - pmin(goals_team_home, goals_team_away) )

R_n <- R_0 + K * (W - W_e)

database_mr <-database_mr %>%  mutate( K = if (goal_diffence == 2) {30*0.5
  } else if (goal_diffence == 3) { 30*3/4
    
  } else if goal_diffence >= 4) { 3/4+ (goal_diffence -3) / 8
    
  }
)#Zunächst keine Gewichtung, da alles Ligaspiele

W = 
  
  lm(f)


