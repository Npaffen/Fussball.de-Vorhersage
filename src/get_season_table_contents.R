# func 1. ======================= start ------------------------------
extract_content <- function(md_url_season , season = season) {
  page <- read_html(md_url_season)

  get_rank <- compose(
    length,
    html_text,
    partial(html_nodes, css = ".column-rank"),
    .dir = "backward"
  )

  get_club_name <- compose(html_text,
    partial(html_nodes, css = ".club-name"),
    .dir = "backward"
  )

  get_games <- compose(html_text,
    partial(html_nodes, css = "td:nth-child(4)"),
    .dir = "backward"
  )

  
  get_wins <- compose(html_text,
                       partial(html_nodes, css = ".row-promotion:nth-child(5)"),
                       .dir = "backward"
  )
  
  
  get_ties <- compose(html_text,
                        partial(html_nodes, css = ".hidden-small:nth-child(6)"),
                      .dir = "backward"
  )
  
  get_loss <- compose(html_text,
                      partial(html_nodes, css = ".hidden-small:nth-child(7)"),
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
    id = season,
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
  df <- tibble(
    season = df$id,
    matchday = df$games,
    rank = df$rank,
    club_name = df$club_name,
    wins = df$wins,
    ties = df$ties,
    loss = df$loss,
    goal_relations = df$goal_relations,
    goal_diff = df$goal_diff,
    points = df$points
  )
  df
}
# func 1. ======================= end -----------------------------------


# func 2. ======================= start ---------------------------
# create a function that extractes the article text data and
# other additional info en mass.

get_article_data <- function(article_urls) {
  len <- length(article_urls$column_url)
  dat <- map(article_urls$column_url, function(x) {
    len <<- len - 1
    if (len %% 10 == 0 || len < 10) {
      message(
        message("GETTING TEXT DATA"),
        "Grab some coffee ", "<", len, "> ",
        "iter", if (len > 1) "s", " left....."
      )
    }
    safely(slowly(extract_content))(x)
    # so that there is a short pause between consecutive requests.
  })

  names(dat) <- article_urls$cols
  dat <- transpose(dat)
  is_ok <- dat$error %>% map_lgl(is_null)
  dat_ok <- suppressWarnings(bind_rows(dat$result[is_ok], .id = "id"))
  dat_notok <- dat$error[!is_ok]
  'dat_ok <- dat_ok %>%
    mutate(
      date = ymd(str_extract(id, "\\d{8}")),
      column_num = str_extract(id, "(_\\d-)"),
      column_num = str_extract(column_num, "\\d"),
      id = str_replace(str_extract(id, "_[\\d_-]+"), "-", "_"),
      page_num = str_extract(id, "\\d{2}$")
    ) %>%
    select(
      id, page_num, date, column_num, title, subtitle,
      content, everything()
    )'
  dat_ok
}


# func 2. ======================= end -------------------------------
