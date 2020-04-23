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
      goals=database_mr$goals_team_home,
      team=database_mr$club_name_home,
      opponent=database_mr$club_name_away,
      home=0)) %>%
  
  glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)
summary(poisson_model)

matchday30 <- database_season %>% filter(season == 1920, matchday == 30)


N <- 5000 # simulation runs
sim_output <- f_score_prob_matches(missinggames, matchday30, poisson_model,max_goals = 10, N, limit = 0.01)
all_final_tables <- sim_output[[1]]
sim_output[[2]] # print convergence plot
average_table <- aggregate(all_final_tables[,-1],
                           by = list(all_final_tables$club_name),
                           FUN = "mean")
# plot selection
dfa <- filter(df, club_name %in% c("VfL Ramsdorf","TuS Gahlen", "SV Schermbeck II", "	SV Altendorf-Ulfkotte"))
# rank as lines
rank_plot <- ggplot(dfa, aes(x=run, y=rank, colour = club_name))
rank_plot + geom_line()
# rank as histogram
dfa <- filter(df, club_name %in% c("VfL Ramsdorf"))
rank_plot <- ggplot(dfa, aes(x = factor(rank)))
rank_plot + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title= "VfL Ramsdorf ranking distribution")
# all ranks hist
dfa <- filter(df, club_name %in%
                c("VfL Ramsdorf","TuS Gahlen", "SV Schermbeck II",
                  "SV Altendorf-Ulfkotte"))
ggplot(dfa,aes(x=rank, fill=club_name)) + geom_histogram(alpha=0.25, binwidth = 1)
ggplot(df,aes(x=rank, fill=club_name)) + geom_histogram(alpha=0.25, binwidth = 1)
