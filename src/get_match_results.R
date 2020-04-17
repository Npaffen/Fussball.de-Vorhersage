# func 1. ======================= start ------------------------------
options(stringsAsFactors = FALSE)
f_extract_match_results <- function(md_url_season) {
  Sys.sleep(runif(1, 5L, 10L))
  message(paste("Now scraping ", md_url_season))
  
  
  page <- read_html(md_url_season)

  attr_href <- seq(from = 1,
      to =  page %>%
        rvest::html_nodes(".column-rank") %>%
        rvest::html_attrs() %>%
        length() %>%
        as.integer()*2-4,
      by = 4)
  
  
  href_list <- map(attr_href, ~html_nodes(page, ".column-detail") %>%
                     .[[.x]] %>%
                     xml_child( 1) %>%
                     xml_attrs("href")
                   ) %>%
    unlist
  
  
  
  get_club_name_home <- compose(html_text,
                                partial(html_nodes, css = ".team-home .team-name a"),
                                .dir = "backward"
  )
  
  get_club_name_away <- compose(html_text,
                                partial(html_nodes, css = ".team-away .team-name a"),
                                .dir = "backward"
  )
  
  get_game_info <- compose( partial(html_nodes, css = "#rangescontainer"),
                            .dir = "backward"
  )
  
  

  
get_results <- function(href_list, md_url_season ){
  Sys.sleep(runif(1, 5L, 10L))
  page <- read_html(href_list)
  
  
  
  if( page %>%
      html_nodes(".info-text") %>% 
      html_text() %>%
      is_empty() == TRUE  ){ #Ist bei Spielen mit Ergebnis immer leer
  df <- list(
    club_name_home = get_club_name_home(page),
    club_name_away = get_club_name_away(page),
    
   
    
    goals_team_a = get_game_info(page) %>%
      xml_attrs() %>%
      .[[1]] %>%
      .[["data-match-events"]] %>%
      str_count( pattern  = "'goal','team':'home'"),
    
    
    goals_team_b = get_game_info(page) %>%
      xml_attrs() %>%
      .[[1]] %>%
      .[["data-match-events"]] %>%
      str_count( pattern  = "'goal','team':'away'") 
  )
    } else { #Falls es kein Ergebnis gibt aufgrund wird Kein Ergebnis eingetragen. Diese Spiele können später noch manuell nachgetragen werden.
      df <- list(
        club_name_home = get_club_name_home(page),
        club_name_away = get_club_name_away(page),
        
      goals_team_a = "Kein Ergebnis",
      goals_team_b = "Kein Ergebnis"
      )
      
    }
    
  
  df_tb <- tibble(
    season = stringr::str_extract(md_url_season, "([0-9]{4}-)") %>%
      gsub(x = ., pattern = "-", replacement = ""),
    matchday = stringr::str_extract(md_url_season, "(\\/[0-9]{1,2}\\/)") %>%
      stringr::str_extract("[0-9]{1,2}"),
    club_name_home = df$club_name_home %>% gsub(x = .,  pattern = "\\n(\\t)+", replacement = ""),
    club_name_away = df$club_name_away%>% gsub(x = .,  pattern = "\\n(\\t)+", replacement = "") ,
    goals_team_home = df$goals_team_a,
    goals_team_away = df$goals_team_b
    
    
   
  ) 
  df_tb
  }
df_games <- map_df(href_list, ~get_results(href_list = .x, md_url_season = md_url_season))
  
  df_games
}
# func 1. ======================= end -----------------------------------

#


