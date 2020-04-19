# func 1. ======================= start ------------------------------
options(stringsAsFactors = FALSE)
f_extract_missing_matches <- function(md_url_season) {
  Sys.sleep(runif(1, 5L, 10L))
  message(paste("Now scraping ", md_url_season))
  
  
  page <- read_html(md_url_season)

  attr_href <- seq(from = 1,
      to =  page %>%
        rvest::html_nodes("#matches .column-club") %>%
        length() %>%
        as.integer()*2-2,
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
  

  
  

  
get_results <- function(href_list, md_url_season ){
  Sys.sleep(runif(1, 5L, 10L))
  page <- read_html(href_list)

     
       df <- list(
        club_name_home = get_club_name_home(page),
        club_name_away = get_club_name_away(page)#,
        
        
      
      )
      
    
    
  
  df_tb <- tibble(
    season = stringr::str_extract(md_url_season, "([0-9]{4}-)") %>%
      gsub(x = ., pattern = "-", replacement = ""),
    matchday = stringr::str_extract(md_url_season, "(\\/[0-9]{1,2}\\/)") %>%
      stringr::str_extract("[0-9]{1,2}"),
    club_name_home = df$club_name_home %>% gsub(x = .,  pattern = "\\n(\\t)+", replacement = ""),
    club_name_away = df$club_name_away%>% gsub(x = .,  pattern = "\\n(\\t)+", replacement = "") 

    
   
  ) 
  df_tb
  }
df_games <- map_df(href_list, ~get_results(href_list = .x, md_url_season = md_url_season))
  
  df_games
}
# func 1. ======================= end -----------------------------------

#


