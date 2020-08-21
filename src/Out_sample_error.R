#Preparation

require(dplyr)
require(purrr)

#Out of Sample Error Poisson 18/19
database_season <- readRDS(here::here("/data/database_season_1819.rds"))  

source(here::here("src/functions_N.R"))




#Out of Sample Error point 16/17
oose <- function(season, sim_type = c('poisson_score', 'elo_ties', 'point')){
database_season <- readRDS(here::here(paste0('data/database_season_',season,'.rds')))  



sim_output <- readRDS(paste0(getwd(),'/data/',sim_type,'_simulation_',season,'.rds'))


# 2. evaluate result

all_final_tables <- sim_output$all_final_tables
all_final_tables <- add_run_rank_col(x = all_final_tables)
all_avg_tables <- sim_output$all_avg_tables
all_avg_tables <- add_run_rank_col(x = all_avg_tables)

# average table result
average_table <- all_avg_tables[
  (nrow(all_avg_tables)-16):nrow(all_avg_tables),]
average_table <- average_table %>% arrange(rank)


#View(average_table)

#level the score for comparison
average_table$score <- floor(average_table$score)

#load the real results
real_results <- database_season %>%
  filter(matchday == 30) %>%
  select(season, matchday, rank, club_name, points)  
  if(season ==  '1617'){real_results <- real_results[1:17,]}
  #Remopve wrong Westfalia Geemn II
  if(nrow(filter(average_table, club_name == "Westfalia Gemen II"))==2){
  average_table <- average_table %>% arrange(desc(run)) %>% .[1:(nrow(average_table)-1),]} else{}

# remove VfB because they never played a match

#calculate out of sample error for 17/18
oose <-  sum((average_table$club_name != real_results$club_name))/nrow(real_results)

return(oose)


}
season <- rep(c('1617', '1718', '1819'),3)
sim_type <- rep(c('poisson_score', 'elo_ties', 'point'),each = 3)

map2(season, sim_type, ~oose(.x, .y))


