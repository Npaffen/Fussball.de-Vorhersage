#Out of Sample Error 18/19
database_season <- readRDS(here::here("/data/database_season_1819.rds"))  

source(here::here("src/functions_N.R"))

sim_output <- readRDS(paste0(getwd(), "/data/poisson_score_simulation_1819.rds"))

# 2. evaluate result

all_final_tables <- sim_output$all_final_tables %>% rename(score = points)
all_final_tables <- add_run_rank_col(x = all_final_tables)
all_avg_tables <- sim_output$all_avg_tables%>%  rename(score = points)
all_avg_tables <- add_run_rank_col(x = all_avg_tables)

# average table result
average_table <- all_avg_tables[
  (nrow(all_avg_tables)-15):nrow(all_avg_tables),] 
average_table <- average_table %>%
  arrange(rank)

print(average_table)
#View(average_table)

#level the score for comparison
average_table$score <- floor(average_table$score)

#load the real results
real_results <- database_season %>%
  filter(matchday == 30) %>% 
  select(season, matchday, rank, club_name, points)
#calculate out of sample error für 18/19
(oose_rank_1819 <- sum((average_table$club_name != real_results$club_name))/nrow(real_results))

(oos_points_1819 <- sum((average_table$points != real_results$points))/nrow(real_results))

sim_compare <- average_table %>%
  select(rank, club_name, score) %>%
  rename(points_sim = score, rank_sim = rank)

sim_vs_real <- real_results %>%
  select(rank, club_name, points) %>% 
  rename(points_real = points, rank_real = rank) %>% 
  dplyr::inner_join(sim_compare,by =  'club_name') %>%
  select(rank_real, rank_sim, everything())

#Out of Sample Error 17/18

database_season <- readRDS(here::here("/data/database_season_1718.rds"))  



sim_output <- readRDS(paste0(getwd(), "/data/poisson_score_simulation_1718.rds"))

# 2. evaluate result

all_final_tables <- sim_output$all_final_tables %>% rename(score = points)
all_final_tables <- add_run_rank_col(x = all_final_tables)
all_avg_tables <- sim_output$all_avg_tables%>%  rename(score = points)
all_avg_tables <- add_run_rank_col(x = all_avg_tables)

# average table result
average_table <- all_avg_tables[
  (nrow(all_avg_tables)-15):nrow(all_avg_tables),] 
average_table <- average_table %>% arrange(rank)

print(average_table)
#View(average_table)

#level the score for comparison
average_table$score <- floor(average_table$score)

#load the real results
real_results <- database_season %>% filter(matchday == 30) %>% select(season, matchday, rank, club_name, points)
#calculate out of sample error für 17/18
(oose_rank_1718 <-sum((average_table$club_name != real_results$club_name))/nrow(real_results))


