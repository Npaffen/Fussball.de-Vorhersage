library(readr)
library(tidyverse)
database_mr <- read_rds(str_c(here::here() , "data", "database_match_results_1920.rds", sep = "/"))

missinggames <- read_rds(here::here("/data/database_missing_matches_1920.rds"))

database_season <- readRDS(here::here("/data/database_season_1920.rds"))

source(here::here("src/functions_N.R"))
#Idee für Untenschieden : Ausrechen wie häufig unentschieden in dieser Saison gespielt wurde. 
#Annahme : Zwei gleichstarke Mannschaften haben eine höhere Wahrscheinlichkeit unentschieden zu spielen als zwei unterschiedlich Starke.
#Ableitung : Für gleichstarke Mannschaften Durchschnittswert für unterschiedlich Starke den Wert diskontieren 
#Proportion zum Rankingunterschied. 












played_matchdays <- database_mr %>% filter(between(matchday,1,20))


#create ratings for the first 20 matchdays

  rating <- f_rating(played_matchdays)

matchday30 <- database_season %>% filter(matchday == 30)

if(0){
N = 5000
sim_output <- f_rating_prob_matches( missinggames, matchday30, rating, ties = T, N, limit = 0.01 )
saveRDS(sim_output, paste0(getwd(), "/data/elo_ties_simulation.rds"))
}
sim_output <- readRDS(paste0(getwd(), "/data/elo_ties_simulation.rds"))




# 2. evaluate result

all_final_tables <- sim_output$all_final_tables %>% rename(score = points)
all_final_tables <- add_run_rank_col(x = all_final_tables)
all_avg_tables <- sim_output$all_avg_tables  %>% select(Group.1, points) %>%rename(score = points,club_name = Group.1 ) 
all_avg_tables <- add_run_rank_col(x = all_avg_tables)

# average table result
average_table <- all_avg_tables[
  (nrow(all_avg_tables)-15):nrow(all_avg_tables),]
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


# convergence
make_plot(x = all_avg_tables, y = "rank",
          club_names, type = "line")
make_plot(x = all_avg_tables, y = "score",
          club_names, type = "line")


##### Elo rating without ties

if(0){
  N = 5000
  sim_output <- f_rating_prob_matches( missinggames, matchday30, rating, ties = F, N, limit = 0.01 )
  saveRDS(sim_output, paste0(getwd(), "/data/elo_simulation.rds"))
}
sim_output <- readRDS(paste0(getwd(), "/data/elo_simulation.rds"))


# 2. evaluate result

all_final_tables <- sim_output$all_final_tables %>% rename(score = points)
all_final_tables <- add_run_rank_col(x = all_final_tables)
all_avg_tables <- sim_output$all_avg_tables%>% rename(score = points)
all_avg_tables <- add_run_rank_col(x = all_avg_tables)

# average table result
average_table <- all_avg_tables[
  (nrow(all_avg_tables)-15):nrow(all_avg_tables),]
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
