#Preparation

require(dplyr)
require(purrr)

source(here::here("src/functions.R"))




#Out of Sample Error 
oose <- function(season, sim_type = c('poisson_score', 'elo_ties', 'point')){
database_season <- readRDS(here::here(paste0('data/database_season_',season,'.rds')))  

if(exists('OOSE_DF') == F ) {OOSE_DF <- tibble(sim = c(1,2,3))}

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

season <- c('1617', '1718', '1819')
sim_type <-c('poisson_score', 'elo_ties', 'point')
season_map <- rep(season,3)
sim_type_map <- rep(sim_type,each = 3)

OOSE <- map2_df(season_map, sim_type_map, ~oose(.x, .y)) %>% 
  unlist() 

OOSE_DF <- tibble(a = OOSE[1:3], b = OOSE[4:6], c = OOSE[7:9] ) %>% setNames(sim_type)

