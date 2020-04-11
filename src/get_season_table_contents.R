# func 1. ======================= start ------------------------------
f_extract_season <- function(md_url_season) {
  Sys.sleep(runif(1, 5L, 10L))
  message(paste("Now scraping ", md_url_season))
  page <- read_html(md_url_season)

  get_rank <- compose(
    length,
    html_text,
    partial(html_nodes, css = ".column-rank"),
    .dir = "backward"
  )

  get_club_name <- compose(html_text,
    partial(html_nodes, css = ".club-name"),
    partial(html_nodes, css = "#fixture-league-tables"),
    .dir = "backward"
  )

  get_games <- compose(html_text,
    partial(html_nodes, css = "td:nth-child(4)"),
    partial(html_nodes, css = "#fixture-league-tables"),
    .dir = "backward"
  )

  
  get_wins <- compose(html_text,
                      partial(html_nodes, css = "td:nth-child(5)"),
                      partial(html_nodes, css = "#fixture-league-tables"),
                       .dir = "backward"
  )
  
  
  get_ties <- compose(html_text,
                      partial(html_nodes, css = "td:nth-child(6)"),
                      partial(html_nodes, css = "#fixture-league-tables"),
                      .dir = "backward"
  )
  
  get_loss <- compose(html_text,
                      partial(html_nodes, css = ".hidden-small:nth-child(7)"),
                      partial(html_nodes, css = "#fixture-league-tables"),
                      .dir = "backward"
  )

  
  get_goal_relations <- compose(html_text,
                      partial(html_nodes, css = ".no-wrap"),
                      .dir = "backward"
  )
  
  
  get_goal_diff <-  compose(html_text,
                            partial(html_nodes, css = ".hidden-small:nth-child(9)"),
                            .dir = "backward"
  )
  
  get_points <- compose(html_text,
                                partial(html_nodes, css = ".column-points"),
                                .dir = "backward"
  )
  
  
  df <- list(
    rank = get_rank(page) %>% 1:.,
    club_name = get_club_name(page) %>% gsub(x = .,  pattern = "\\n(\\t)+", replacement = ""),
    games = get_games(page),
    wins = get_wins(page),
    ties = get_ties(page),
    loss = get_loss(page),
    goal_relations = get_goal_relations(page),
    goal_diff = get_goal_diff(page),
    points = get_points(page)
  )
  df_tb <- tibble(
    season = stringr::str_extract(md_url_season, "([\\d]{4}-)") %>%
      gsub(x = ., pattern = "-", replacement = "")%>%
      rep(length(df$rank)),
    matchday <- stringr::str_extract(md_url_season, "(\\/[0-9]{1,2}\\/)") %>%
      stringr::str_extract("[0-9]{1,2}") %>%
      rep(length(df$rank)),
    games = df$games,
    rank = df$rank,
    club_name = df$club_name,
    wins = df$wins,
    ties = df$ties,
    loss = df$loss,
    goal_relations = df$goal_relations,
    goal_diff = df$goal_diff,
    points = df$points
  ) 
  
  
  
  df_tb
}
# func 1. ======================= end -----------------------------------

#


