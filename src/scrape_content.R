# load packages -------------------------------------------------
library(lubridate)
library(rvest)
library(tidyverse)
library(glue)
# ---------------------------------------------------------------
# if dates is not supplied, then ... passes args to the make_dates()
# function, which are [ year, month, from_day, to_day, all_dates (logical),
# respectively ]. Please refer to `make_dates.R`.
seasons <-  "1617"

seasonsID <- "01SNI60QL4000007VS54898EVT9SILN7"
              
source(here::here("src/functions.R"))
md_season_url <- map2(.x = seasons, .y = seasonsID, ~ f_md_url_season(season = .x, seasonID = .y)) %>%
  unlist()

  # Download season table content ----------------------------------------------
  source("src/get_season_table_contents.R")
  message(paste("Loaded game days: ", length(md_season_url),
                "\n Will begin scraping..."))
  database_season <-  map_df(md_season_url, ~f_extract_season(md_url_season = .x ))  %>% 
    mutate(season = as.numeric(season),
           matchday = as.numeric(matchday),
           goal_diff = as.numeric(goal_diff),
           points = as.numeric(points),
           wins = as.numeric(wins),
           ties = as.numeric(ties),
           loss = as.numeric(loss))
  saveRDS(database_season, here::here("/data/database_season_1617.rds"))
  
  #Download match results 

    database_match_results <-  map_df(md_season_url, ~f_extract_match_results(md_url_season = .x ))
    
      #Some games cant be scraped due to a missing node but results are still there. Added them manually
    database_match_results <- database_match_results %>% 
      #filter(is.na(goals_team_home) == T | is.na(goals_team_away) == T ) %>%
      #mutate(goals_team_home = c(2, 2, 0, 0, NA, NA)) %>%
      #mutate(goals_team_away = c(0, 0, 2, 2, NA, NA)) %>% 
      #bind_rows(database_match_results) %>%
      drop_na() %>%
      mutate(matchday = as.numeric(matchday),
             season = as.numeric(season))
      
    database_match_results <- database_match_results %>%
      setdiff(x = ., y = database_mr[duplicated(database_mr[,c("club_name_home", "club_name_away")]),])
      
    saveRDS(database_match_results, here::here("/data/database_match_results_1617.rds"))
    
    
#Download missing matches. Need to insert only urls of missing matchdays

  database_missing_matches_1920 <- map_df(md_season_url[21:30], ~f_extract_missing_matches(md_url_season = .x ))
  saveRDS(database_missing_matches_1920, here::here("/data/database_missing_matches_1920.rds"))

  