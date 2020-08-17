library(readr)
library(tidyverse)
database_mr <- readRDS(here::here("/data/database_match_results_1617.rds"))[1:175,] 

missinggames <- read_rds(here::here("/data/database_match_results_1617.rds"))[176:289,1:4]

database_season <- readRDS(here::here("/data/database_season_1617.rds"))  

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

matchday30 <- database_season %>% filter(season == '1617', matchday == 34)

if(0){
N <- 5000 # simulation runs
sim_output_poisson <- f_score_prob_matches(missinggames, matchday30, poisson_model,max_goals = 10, N, limit = 0.01)
saveRDS(sim_output_poisson, paste0(getwd(), "/data/poisson_score_simulation_1617.rds"))
}
sim_output <- readRDS(paste0(getwd(), "/data/poisson_score_simulation.rds"))

# 2. evaluate result

all_final_tables <- sim_output$all_final_tables %>% rename(score = points)
all_final_tables <- add_run_rank_col(x = all_final_tables)
all_avg_tables <- sim_output$all_avg_tables%>%  rename(score = points, club_name = Group.1 )
all_avg_tables <- add_run_rank_col(x = all_avg_tables)

# average table result
average_table <- all_avg_tables[
  (nrow(all_avg_tables)-15):nrow(all_avg_tables),] 
average_table <- average_table %>% arrange(rank)

print(average_table)
#View(average_table)



## 3. calculate plots


# compare some clubs
club_names <- c("SV Altendorf-Ulfkotte",
                "SV Schermbeck II",
                "TuS Gahlen",
                "VfL Ramsdorf")

# track one club
club_names <- "VfL Ramsdorf"

# all
club_names <- unique(database_season$club_name)

# run plots

# rankings
make_plot(x = all_final_tables, y = "rank",
          club_names, type = "hist")
make_plot(x = all_final_tables, y = "rank",
          club_names, type = "line")

# final table score
make_plot(x = all_final_tables, y = "score",
          club_names, type = "hist")
make_plot(x = all_final_tables, y = "score",
          club_names, type = "line")

# final table goal difference
make_plot(x = all_final_tables, y = "goal_diff",
          club_names, type = "hist")
make_plot(x = all_final_tables, y = "goal_diff",
          club_names, type = "line")

# convergence
make_plot(x = all_avg_tables, y = "rank",
          club_names, type = "line")
make_plot(x = all_avg_tables, y = "score",
          club_names, type = "line")

    