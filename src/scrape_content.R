# load packages -------------------------------------------------
library(lubridate)
library(rvest)
library(tidyverse)
library(glue)
# ---------------------------------------------------------------
# if dates is not supplied, then ... passes args to the make_dates()
# function, which are [ year, month, from_day, to_day, all_dates (logical),
# respectively ]. Please refer to `make_dates.R`.
seasons <- c("1617", "1718", "1819", "1920")
seasonsID <- c("01SNI60QL4000007VS54898EVT9SILN7", "0209KP9SAC00000AVS54898DVUVCCN5J",
               "023VNLHG8000000CVS54898DVVG1IBJM", "027II28DH8000009VS5489B3VS3GHJJU")
source("src/Scrape_RL_Recklingh_A1_Kreis_Recklingh_A_Herren.R")
md_season_url <- map2(.x = seasons, .y = seasonsID, ~ f_url_md_season(season = .x, seasonID = .y)) %>%
  unlist()

  # Download season table content ----------------------------------------------
  source("src/get_season_table_contents.R")
  message(paste("Loaded game days: ", length(md_season_url),
                "\n Will begin scraping..."))
  database_season <-  map_df(md_season_url, ~f_extract_season(md_url_season = .x ))
  saveRDS(database_season, here::here("/data/database_season.rds"))

  season_1617 <- database_season %>% filter(season == "1617")
  season_1718 <- database_season %>% filter(season == "1718")
  season_1819 <- database_season %>% filter(season == "1819")
  season_1920 <- database_season %>% filter(season == "1920")
  


  database_season <- readRDS(here::here("/data/database_season.rds"))

  #Download match results 
  if(0){ 
    source("src/get_match_results.R")
    database_match_results <-  map_df(md_season_url, ~f_extract_match_results(md_url_season = .x ))
    saveRDS(database_match_results, here::here("/data/database_match_results.rds"))
    
  }
  
  library(RMariaDB)
  
  
  