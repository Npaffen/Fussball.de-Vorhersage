# load packages -------------------------------------------------
library(lubridate)
library(rvest)
library(tidyverse)
library(glue)
# ---------------------------------------------------------------
# if dates is not supplied, then ... passes args to the make_dates()
# function, which are [ year, month, from_day, to_day, all_dates (logical),
# respectively ]. Please refer to `make_dates.R`.
seasons <- c( "1112", "1213", "1314", "1415", "1516", "1617", "1718", "1819", "1920")

seasonsID <- c( "01AE4TLH9O000000VV0AG813VS3VU5IB","01DVBP066S000000VV0AG812VS2KTNCU" ,
                "01HLF6ON2G000000VV0AG812VS2HNECA" , "01L8MF7QI4000001VV0AG812VVHQG9J2" ,
                "01OVSEG8F0000009VV0AG813VS7LCAJK", "01SNI60QL4000007VS54898EVT9SILN7",
                "0209KP9SAC00000AVS54898DVUVCCN5J", "023VNLHG8000000CVS54898DVVG1IBJM",
                "027II28DH8000009VS5489B3VS3GHJJU")
source("src/Scrape_RL_Recklingh_A1_Kreis_Recklingh_A_Herren.R")
md_season_url <- map2(.x = seasons[9], .y = seasonsID[9], ~ f_url_md_season(season = .x, seasonID = .y)) %>%
  unlist()

  # Download season table content ----------------------------------------------
  source("src/get_season_table_contents.R")
  message(paste("Loaded game days: ", length(md_season_url),
                "\n Will begin scraping..."))
  database_season <-  map_df(md_season_url, ~f_extract_season(md_url_season = .x ))
  saveRDS(database_season, here::here("/data/database_season_0809_1516.rds"))

  season_1617 <- database_season %>% filter(season == "1617")
  season_1718 <- database_season %>% filter(season == "1718")
  season_1819 <- database_season %>% filter(season == "1819")
  season_1920 <- database_season %>% filter(season == "1920")
  
  #Download match results 
    source("src/get_match_results.R")
    database_match_results <-  map_df(md_season_url, ~f_extract_match_results(md_url_season = .x ))
    saveRDS(database_match_results, here::here("/data/database_match_results_0809_1516.rds"))
    
#Download missing matches. Need to insert only urls of missing matchdays
    source(here::here('src/get_missing_matches.R'))
  database_missing_matches_1920 <- map_df(md_season_url[21:30], ~f_extract_missing_matches(md_url_season = .x ))
  saveRDS(database_missing_matches_1920, here::here("/data/database_missing_matches_1920.rds"))

  