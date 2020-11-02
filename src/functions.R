## functions for fussball.de vorhersage

require(dplyr)
require(purrr)
require(ggplot2)

# add_run_rank_rank_col(), adds run and rank column to all_tables output
# find_games_played(), finds all games that were played
# make_all_games(), creates list with all matchups
# make_plot(), makes output plots
# sim_points(), calculates the winning probabilty of the home team using points
###########################################################
#Scrape_RL_Recklingh_A1_Kreis_Recklingh_A_Herren

#generate matchday links
f_md_url_season <- function( season ="1617", seasonID = "0209KP9SAC00000AVS54898DVUVCCN5J") {
  matchdays <-xml2::read_html(stringr::str_c('http://www.fussball.de/spieltag/re-kl-a-1-kreis-recklinghausen-kreisliga-a-herren-saison',
                                             season, '-westfalen',
                                             '/-/spieltag/1/staffel/', seasonID,'-G', '#', '!/', sep = "")) %>%
    rvest::html_nodes(".column-rank") %>%
    rvest::html_attrs() %>%
    length() %>%
    as.integer()*2-2
  
  
  md_url <- purrr::map(1:matchdays, #
                       ~stringr::str_c('http://www.fussball.de/spieltag/re-kl-a-1-kreis-recklinghausen-kreisliga-a-herren-saison', season,
                                       '-westfalen/-/spieltag/', .x, '/staffel/', seasonID,'-G', '#', '!/', sep = ""))
  
  md_url
}

############################################################
#get match results
options(stringsAsFactors = FALSE)
f_extract_match_results <- function(md_url_season) {
  Sys.sleep(runif(1, 5L, 10L))
  message(paste("Now scraping ", md_url_season))
  
  
  page <- read_html(md_url_season)
  
  attr_href <- seq(from = 1,
                   to =  page %>%
                     rvest::html_nodes("#matches .column-club") %>%
                     length() %>%
                     as.integer()*2-2,
                   by = 4)
  
  #Prüfe ob ein Team spielfrei hat und lösche dieses Element aus attr_href
  nomatch_checker <- page %>% html_nodes(' .no-border') %>% html_text() %>% grep(pattern = 'spielfrei')
  
  if(identical(nomatch_checker, integer(0)) == F ){attr_href <- attr_href[!attr_href %in% attr_href[nomatch_checker/2]]}
  
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
    page <- read_html(href_list[1])
    
    if ( page %>%
         html_nodes(".info-text") %>% 
         html_text() %>%
         is_empty() == FALSE  ){ #Ist bei bereits ausgetragenen Spielen ohne Ergebnis niemals leer. 
      #Falls es kein Ergebnis gibt wird NA eingetragen. Diese Spiele können später noch manuell nachgetragen werden.
      df <- list(
        club_name_home = get_club_name_home(page),
        club_name_away = get_club_name_away(page),
        
        goals_team_a = NA,
        goals_team_b = NA
      ) 
      
    }else if (page %>%
              html_nodes("span:nth-child(4)") %>% .[1] %>% 
              gsub(x = . , pattern = '<span>\\s|\\s</span>', replacement = "") =="U"){ # Spiel wurde uneideutig gewertet
      df <- list(
        club_name_home = get_club_name_home(page),
        club_name_away = get_club_name_away(page),
        
        goals_team_a = NA,
        goals_team_b = NA
      )
      
      message(paste("Please revist", href_list ))
    }else if (page %>%
              html_nodes("sub") %>%
              html_text() %>% identical(character(0)) == T){ #Kein Ergebnis für das Spiel vorhanden
      df <- list(
        club_name_home = get_club_name_home(page),
        club_name_away = get_club_name_away(page),
        
        goals_team_a = NA,
        goals_team_b = NA
      )
      
      message(paste("Please revist", href_list ))
    }else if (page %>%
              html_nodes("sub") %>%
              html_text() %>%
              gsub(x = ., pattern = "\\s", replacement = "" )== "W"){ #Spiel wurde wiederholt
      df <- list(
        club_name_home = get_club_name_home(page),
        club_name_away = get_club_name_away(page),
        
        goals_team_a = NA,
        goals_team_b = NA
      )
      message(paste("Please revist", href_list ))
    }  else { 
      
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

############################################################
## add_rank_col(), adds run and rank column to all_tables output

add_run_rank_col <- function(x = all_final_tables){
  x$run <- rep(1:(length(x$score)/16), each = 16)
  x$rank <- NA
  for(i in 1:(length(x$score)/16)){
    tab <- x[x$run == i,]
    tab <- transform(tab, rank = rank(-score, ties.method = "first"))
    x[x$run == i,] <- tab
  }
  return(x)
}
############################################################
#get missing matches
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

############################################################
#get content of the leagues season table for each matchday
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
    matchday = stringr::str_extract(md_url_season, "(\\/[0-9]{1,2}\\/)") %>%
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

############################################################
## find_games_played(), finds all games that were played

find_games_played <- function(dbs_data, dbm_data, season){
  games_played <- data.frame(club_name_home = as.character(),
                             club_name_away = as.character(),
                             stringsAsFactors = FALSE)
  for(i in unique(dbs_data$club_name)){
    a <- dbm_data %>%
      filter(season == season, club_name_home == i) %>%
      select(club_name_away) %>%
      unique()
    b <- dbm_data %>%
      filter(season == season, club_name_away == i) %>%
      select(club_name_home) %>%
      unique()
    if(nrow(a) != 0){
      c <- data.frame(club_name_home = i,
                    club_name_away = a,
                    stringsAsFactors = FALSE)
    }
    if(nrow(b) != 0){
      d <- data.frame(club_name_home = b,
                      club_name_away = i,
                      stringsAsFactors = FALSE)
    }
    games_played <- bind_rows(games_played, c, d)
  }
  return(games_played)
}


############################################################
## make_all_games(), creates list with all matchups

make_all_games <- function(dbs_data, games_played){
  all_games <- games_played[0,]
  for(i in unique(dbs_data$club_name)){
    a <- data.frame(club_name_home = i,
                    club_name_away = unique(dbs_data$club_name),
                    stringsAsFactors = FALSE)
    c <- a[a$club_name_home != a$club_name_away,]
    all_games <- bind_rows(all_games, c)
  }
  return(all_games)
}



############################################################
## make_plot(), makes output plots

make_plot <- function(x = all_final_tables,
                      y = "rank",
                      club_names = c("SV Altendorf-Ulfkotte",
                                     "SV Schermbeck II",
                                     "TuS Gahlen",
                                     "VfL Ramsdorf"),
                      type = "hist"){
  dfa <- x
  dfa <- filter(dfa, club_name %in% club_names)
  if(type == "line"){
    make_plot <- ggplot(dfa, aes_string(x="run", y=y, colour = "club_name"))
    return(make_plot + geom_line())
  }
  if(type == "hist"){
    make_plot <- ggplot(dfa, aes_string(x=y, fill="club_name"))
    return(make_plot + 
             geom_bar(aes(y = (..count..)/sum(..count..))) + 
             scale_y_continuous(labels = scales::percent)+
             labs(title= paste0(y, " histogram")) 
    )
  }
}




############################################################
# run_points_sim() runs N simulation runs for the missing games

run_points_sim <- function(missing_games, dbs1920, 
                           win_prob_home, sim_results, N, limit){
  old_table <- data.frame(club_name = dbs1920$club_name,
                          wins = dbs1920$wins,
                          losses = dbs1920$loss,
                          score = dbs1920$points,
                          stringsAsFactors = FALSE)
  # simulated games
  sim_results <- missing_games 
  # winning probabilites based on points
  win_prob_home <- map2(
    sim_results$club_name_home,
    sim_results$club_name_away,
    ~sim_points(.x, .y, dbs_data = dbs1920)
  )
  win_prob_home <- unlist(win_prob_home)
  # simulated final season table
  all_final_tables <- data.frame(stringsAsFactors = FALSE)
  all_avg_tables <- data.frame(stringsAsFactors = FALSE)
  clubs <- unique(missing_games$club_name_home)
  # track convergence speed
  conv <- numeric()
  # run simulation
  for(i in 1:N){
    draw_result <- rbinom(nrow(missing_games), size = 1, p=win_prob_home)
    sim_results$score_home <- draw_result*3
    sim_results$score_away <- (1-draw_result)*3
    final_table <- data.frame(club_name = dbs1920$club_name, stringsAsFactors = FALSE)
    final_table$wins <- map_int(final_table$club_name, function(.x){
      nrow(filter(sim_results,
                  sim_results$club_name_away == .x,
                  sim_results$score_away != 0)) +
        nrow(filter(sim_results,
                    sim_results$club_name_home == .x,
                    sim_results$score_home != 0))
    })
    final_table$losses <- map_int(final_table$club_name, function(x){
      nrow(filter(sim_results,
                  sim_results$club_name_away == x,
                  sim_results$score_away == 0)) +
        nrow(filter(sim_results,
                    sim_results$club_name_home == x,
                    sim_results$score_home == 0))
    })
    final_table$score <- final_table$wins*3
    # sort final_table and add results from games_played
    final_table <- final_table[order(final_table$club_name),]
    old_table <- old_table[order(old_table$club_name),]
    final_table[,2:4] <- final_table[,2:4]+old_table[,2:4]
    # record run results
    all_final_tables <- bind_rows(all_final_tables, final_table)
    # calculate average of all runs so far to calculate conv speed
    average_table <- aggregate(
      all_final_tables[1:(length(clubs)*(i-1)),-1],
      by = list(all_final_tables$club_name[1:(length(clubs)*(i-1))]),
      FUN = "mean"
    )
    average_table2 <- aggregate(
      all_final_tables[,-1],
      by = list(all_final_tables$club_name),
      FUN = "mean"
    )
    all_avg_tables <- bind_rows(all_avg_tables, average_table2)
    conv_speed <- sum(abs(average_table$score-average_table2$score))
    conv <- c(conv, conv_speed)
    # report speed and progress
    if(i %% 10 == 0){
      message("convergence speed: ", round(conv_speed, 3), " run ", i, " out of ", N)
    }
    if(conv_speed < limit){
      message("converged!")
      break
    }
  }
  # save convergence plot
  if(conv_speed < limit){
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("converged to below ", limit, 
                                     " after ", length(conv), " runs"))
  } else {
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("didn't converge below ", limit, 
                                     " after ", N, " runs"))
  }
  all_avg_tables <- rename(all_avg_tables, club_name = Group.1)
  sim_output <- list("all_final_tables" = all_final_tables,
                     "conv_plot" = conv_plot,
                     "sim_results" = sim_results,
                     "win_prob_home" = cbind(missing_games,win_prob_home=win_prob_home),
                     "all_avg_tables" = all_avg_tables
                     )
  return(sim_output)
}



############################################################
# sim_points(), calculates the winning probabilty of the home team using points

sim_points <- function(x,y, dbs_data){
  a <- dbs_data[dbs_data$club_name == x,]$points
  b <- dbs_data[dbs_data$club_name == y,]$points
  return(a/(a+b))
}
 



##### rating  for the first 20 matchdays (real data)

f_rating <- function(played_matchdays){
  
  #Füge Tordifferenz hinzu
  rating <- tibble( teams = unique(played_matchdays$club_name_home), rating_value_home = 1000, rating_value_away = 1000)
  
  played_matchdays<- played_matchdays %>%
    mutate(goal_difference = pmax(goals_team_home, goals_team_away) - pmin(goals_team_home, goals_team_away) )
  
  
  
  
  
  played_matchdays <- played_matchdays %>%  mutate(K = case_when(goal_difference <= 1 ~ 30,
                                                                 goal_difference == 2 ~ 30+0.5,
                                                                 goal_difference == 3 ~ 30+3/4,
                                                                 goal_difference >= 4 ~ 30+ 3/4+ (goal_difference -3) / 8,
                                                                 TRUE ~ 1),
                                                   W_team_home = case_when( pmax(goals_team_home, goals_team_away) == goals_team_home ~ 1,
                                                                            goal_difference == 0 ~ 0.5,
                                                                            TRUE ~ 0),
                                                   W_team_away = case_when(pmax(goals_team_home, goals_team_away) == goals_team_away ~ 1,
                                                                           goal_difference == 0 ~ 0.5,
                                                                           TRUE ~ 0)
  )# K ist der Gewichtungsfaktor pro Spiel pro Tordifferenz
  rating_home <- as.numeric()
  rating_away <- as.numeric()
  dr_home <- as.numeric()
  W_e_home <- as.numeric()
  W_e_away <- as.numeric()
  for (m in seq_along(played_matchdays$club_name_home)){
    rating_home <-  rating %>% filter(teams == played_matchdays$club_name_home[m]) %>% .$rating_value_home
    rating_away <-  rating %>% filter(teams == played_matchdays$club_name_away[m]) %>% .$rating_value_away
    
    dr_home <-rating_home - rating_away +100
    
    dr_away <-  1 - dr_home
    
    W_e_home <- 1 / (10^(-dr_home/400) + 1)
    
    W_e_away <- 1 / (10^(-dr_away/400) + 1)
    
    
    rating$rating_value_home[rating$teams == played_matchdays$club_name_home[m]] <- rating_home + played_matchdays$K[m] * (played_matchdays$W_team_home[m] - W_e_home)
    
    rating$rating_value_away[rating$teams == played_matchdays$club_name_away[m]] <- rating_away + played_matchdays$K[m] * (played_matchdays$W_team_away[m] - W_e_away)
  }
  rating
}


###  creating score probabiblities
f_simulate_score_prob <- function(foot_model, homeTeam, awayTeam){
  
  
  home_goals_avg <- map2_df(.x = homeTeam, .y = awayTeam,~predict(foot_model,
                                                                  data.frame(home=1, team=.x, 
                                                                             opponent=.y), type="response") %>% floor
  )
  away_goals_avg <- map2_df(.x = homeTeam, .y = awayTeam,~predict(foot_model, 
                                                                  data.frame(home=0, team=.y, 
                                                                             opponent=.x), type="response") %>% floor
  )
  goals <- data.frame(home = home_goals_avg$`1`, away = away_goals_avg$`1`)
  return(goals)
  #map(1:nrow(prob_df), ~as.numeric(prob_df[.x,])) %>% unlist()
}



table_update <- function(missinggames, matchday30){
  clubs <- matchday30$club_name
  missinggames <- missinggames %>%
    mutate(points_team_home = case_when(Goals_team_home > Goals_team_away ~3, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 0),
           points_team_away = case_when(Goals_team_home > Goals_team_away ~0, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 3),
           Goal_diff_home = Goals_team_home - Goals_team_away,
           Goal_diff_away = Goals_team_away - Goals_team_home
    )
  
  points_update <- map(clubs, ~ missinggames %>%
                         filter(club_name_home == .x ) %>%
                         dplyr::select(points_team_home) %>%
                         sum()+ missinggames %>% 
                         filter(club_name_away == .x ) %>%
                         dplyr::select(points_team_away) %>% 
                         sum()) %>%
    unlist 
  
  
  
  goal_diff_update <- map(clubs, ~ missinggames %>%
                            filter(club_name_home == .x ) %>%
                            dplyr::select(Goal_diff_home) %>%
                            sum()+ missinggames %>% 
                            filter(club_name_away == .x ) %>%
                            dplyr::select(Goal_diff_away) %>% 
                            sum()) %>%
    unlist 
  #all_scores <- all_scores %>%
  # inner_join(missinggames %>% dplyr::select(Goals_team_away, Goals_team_away)
  # )
  
  final_table <- matchday30 %>%
    mutate( points_new =  points_update,
            points = points + points_new,
            goal_diff_new = goal_diff_update,
            goal_diff = goal_diff + goal_diff_new) %>%
    dplyr::select(club_name, points, goal_diff) %>%
    arrange(desc(points), desc(goal_diff)) %>%
    mutate(rank = 1:length(club_name)) %>%
    dplyr::select(rank, everything())
  
  return(final_table)
  
}




#simulate poisson score prob model
f_score_prob_matches <- function( missinggames, matchday30, foot_model, max_goals = 10, N, limit ){
  all_avg_tables <- data.frame(stringsAsFactors = FALSE)
  all_final_tables <- data.frame(stringsAsFactors = FALSE)
  clubs <- matchday30$club_name
  matchday30_reset <- matchday30
  conv <- numeric()
  all_scores <-tibble(Goals_team_home = 0, Goals_team_away = 0)
  for(i in 1:N){
    Goals_team_away_u <- 0
    Goals_team_home_u <- 0
    matchday30 <- matchday30_reset
    for (m in seq_along(missinggames$club_name_home)){
      score_prob <- f_simulate_score_prob(foot_model = foot_model,
                                          homeTeam = missinggames$club_name_home[m],
                                          awayTeam = missinggames$club_name_away[m],
                                          max_goals = max_goals)
      
      score <- base::sample(x = score_prob, size = 1, prob = score_prob)
      
      Goals_team_home_u[m] <- which(score_prob == score, arr.ind = T)[1]
      
      Goals_team_away_u[m] <- which(score_prob == score, arr.ind = T)[2]
    }
    
    missinggames <- missinggames %>% mutate(Goals_team_home = Goals_team_home_u,
                                            Goals_team_away = Goals_team_away_u,
                                            Goal_diff_home = Goals_team_home_u - Goals_team_away_u,
                                            Goal_diff_away = Goals_team_away_u - Goals_team_home_u)
    
    missinggames <- missinggames %>%
      mutate(points_team_home = case_when(Goals_team_home > Goals_team_away ~3, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 0),
             points_team_away = case_when(Goals_team_home > Goals_team_away ~0, Goals_team_home == Goals_team_away ~ 1, TRUE ~ 3))
    
    
    points_update <- map(clubs, ~ missinggames %>%
                           filter(club_name_home == .x ) %>%
                           dplyr::select(points_team_home) %>%
                           sum()+ missinggames %>% 
                           filter(club_name_away == .x ) %>%
                           dplyr::select(points_team_away) %>% 
                           sum()) %>%
      unlist 
    
    
    
    goal_diff_update <- map(clubs, ~ missinggames %>%
                              filter(club_name_home == .x ) %>%
                              dplyr::select(Goal_diff_home) %>%
                              sum()+ missinggames %>% 
                              filter(club_name_away == .x ) %>%
                              dplyr::select(Goal_diff_away) %>% 
                              sum()) %>%
      unlist 
    #all_scores <- all_scores %>%
    # inner_join(missinggames %>% dplyr::select(Goals_team_away, Goals_team_away)
    # )
    
    final_table <- matchday30 %>%
      mutate( points_new =  points_update,
              points = points + points_new,
              goal_diff_new = goal_diff_update,
              goal_diff = goal_diff + goal_diff_new) %>%
      dplyr::select(club_name, points, goal_diff) %>%
      arrange(desc(points), desc(goal_diff)) 
    
    all_final_tables <- bind_rows(all_final_tables,final_table)
    
    average_table <- aggregate(
      all_final_tables[1:(length(clubs)*(i-1)),-1],
      by = list(all_final_tables$club_name[1:(length(clubs)*(i-1))]),
      FUN = "mean"
    )
    
    average_table2 <- aggregate(
      all_final_tables[,-1],
      by = list(all_final_tables$club_name),
      FUN = "mean"
    )
    all_avg_tables <- bind_rows(all_avg_tables, average_table2)
    conv_speed <- sum(abs(average_table$points-average_table2$points))
    conv <- c(conv, conv_speed)
    if(i %% 10 == 0){
      message("convergence speed: ", round(conv_speed, 3), " run ", i, " out of ", N)
    }
    if(conv_speed < limit){
      message("converged!")
      break
    }
  }
  if(conv_speed < limit){
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("converged to below ", limit, 
                                     " after ", length(conv), " runs"))
  } else {
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("didn't converge below ", limit, 
                                     " after ", N, " runs"))
  }
  all_avg_tables <- rename(all_avg_tables, club_name = Group.1)
  sim_output <- list("all_final_tables" = all_final_tables,
                     "conv_plot" = conv_plot,
                     "all_avg_tables" = all_avg_tables#,
                     #"all_scores" = all_scores
  )
  return(sim_output)
}
#simulation of elo rating model
f_rating_prob_matches <- function( missinggames, matchday30, rating, ties, N, limit, season ){
  all_avg_tables <- data.frame(stringsAsFactors = FALSE)
  all_final_tables <- data.frame(stringsAsFactors = FALSE)
  
  clubs <- matchday30$club_name
  
  matchday30_reset <- matchday30
  
  tie_prob <- database_season %>%
    filter( season == season) %>%
    dplyr::select(ties) %>% 
    sum/length(database_season$ties)
  
  
  conv <- numeric()
  for(i in 1:N){
    Goals_team_away_u <- 0
    Goals_team_home_u <- 0
    matchday30 <- matchday30_reset
    rating_home <- as.numeric()
    rating_away <- as.numeric()
    dr_home <- as.numeric()
    W_e_home <- as.numeric()
    W_e_away <- as.numeric()
    W_team_home <- as.numeric()
    W_team_away <- as.numeric()
    for (m in seq_along(missinggames$club_name_home)){
      rating_home <-  rating %>% filter(teams == missinggames$club_name_home[m]) %>% .$rating_value_home
      rating_away <-  rating %>% filter(teams == missinggames$club_name_away[m]) %>% .$rating_value_away
      
      
      dr_home <- rating_home - rating_away +100
      
      dr_away <-  rating_away - rating_home 
      
      if (ties == T){
        
        W_e_home <- 1 / (10^(-dr_home/400) + 1) - tie_prob/2
        W_e_away <- 1 / (10^(-dr_away/400) + 1) - tie_prob/2
      } else {
        
        W_e_home <- 1 / (10^(-dr_home/400) + 1)
        W_e_away <- 1 / (10^(-dr_away/400) + 1)
      }
      
      if(W_e_home <= 0){
        tie_prob <- tie_prob + W_e_home
        W_e_home <- 0
      }
      
      if(W_e_away <= 0){
        tie_prob <- tie_prob + W_e_away
        W_e_away <- 0
      }
      
      # W_e_away + W_e_home + tie_prob
      
      W_team_home[m] <- base::sample(x = c(3,0,1), size = 1, prob = c(W_e_home, W_e_away,tie_prob))
      
      W_team_away[m] <- if (W_team_home[m] == 3) 0 else if (W_team_home[m] == 1) 1 else 3
      
    }
    
    missinggames <- missinggames %>% mutate(points_team_home = W_team_home,
                                            points_team_away = W_team_away,
    )
    
    
    points_update <- map(clubs, ~ missinggames %>%
                           filter(club_name_home == .x ) %>%
                           dplyr::select(points_team_home) %>%
                           sum()+ missinggames %>% 
                           filter(club_name_away == .x ) %>%
                           dplyr::select(points_team_away) %>% 
                           sum()) %>%
      unlist 
    
    
    final_table <- matchday30 %>%
      mutate( points_new =  points_update,
              points = points + points_new)%>%
      dplyr::select(club_name, points) %>%
      arrange(desc(points)) 
    
    all_final_tables <- bind_rows(all_final_tables,final_table)
    
    average_table <- aggregate(
      all_final_tables[1:(length(clubs)*(i-1)),],
      by = list(all_final_tables$club_name[1:(length(clubs)*(i-1))]),
      FUN = "mean"
    )
    average_table2 <- aggregate(
      all_final_tables,
      by = list(all_final_tables$club_name),
      FUN = "mean"
    )
    all_avg_tables <- bind_rows(all_avg_tables, average_table2)
    conv_speed <- sum(abs(average_table$points-average_table2$points))
    conv <- c(conv, conv_speed)
    if(i %% 10 == 0){
      message("convergence speed: ", round(conv_speed, 3), " run ", i, " out of ", N)
    }
    if(conv_speed < limit){
      message("converged!")
      break
    }
  }
  if(conv_speed < limit){
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("converged to below ", limit, 
                                     " after ", length(conv), " runs"))
  } else {
    conv_plot <- qplot(x=1:length(conv), y=conv, geom = "jitter",
                       main = paste0("didn't converge below ", limit, 
                                     " after ", N, " runs"))
  }
  all_avg_tables <- all_avg_tables %>%  dplyr::select(Group.1 , points) %>% rename(club_name = Group.1, score = points)
  
  sim_output <- list("all_final_tables" = all_final_tables,
                     "conv_plot" = conv_plot,
                     "all_avg_tables" = all_avg_tables
  )
  return(sim_output)
}

############################################################
## add_rank_col(), adds run and rank column to all_tables output

add_run_rank_col <- function(x = all_final_tables){
  rank_count <- length(unique(x$club_name))
  x$run <- rep(1:(length(x$score)/rank_count), each = rank_count)
  x$rank <- NA
  for(i in 1:(length(x$score)/rank_count)){
    tab <- x[x$run == i,]
    tab <- transform(tab, rank = rank(-score, ties.method = "first"))
    x[x$run == i,] <- tab
  }
  return(x)
}
############################################################

## make_plot(), makes output plots

make_plot <- function(x = all_final_tables,
                      y = "rank",
                      club_names = c("SV Altendorf-Ulfkotte",
                                     "SV Schermbeck II",
                                     "TuS Gahlen",
                                     "VfL Ramsdorf"),
                      type = "hist"){
  dfa <- x
  dfa <- filter(dfa, club_name %in% club_names)
  if(type == "line"){
    make_plot <- ggplot(dfa, aes_string(x="run", y=y, colour = "club_name"))
    return(make_plot + geom_line())
  }
  if(type == "hist"){
    make_plot <- ggplot(dfa, aes_string(x=y, fill="club_name"))
    return(make_plot + 
             geom_bar(aes(y = (..count..)/sum(..count..))) + 
             scale_y_continuous(labels = scales::percent)+
             labs(title= paste0(y, " histogram")) 
    )
  }
}

#nbinom plot 
nbinom_plot<- function(sim_years){
  database_mr <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))%>% filter(between(matchday, 1, 20))
  
  database_season <- read_rds(here::here(paste0("/data/database_season_",sim_years,".rds")))
  
  missinggames <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds"))) %>% .[1:4]
  
  
  


  
  
  
  home_goals_avg <- database_mr$goals_team_home %>% mean()
  
  away_goals_avg <- database_mr$goals_team_away %>% mean()
  
  
  nbinom_model <- 
    bind_rows(
      tibble(
        goals = database_mr$goals_team_home,
        team = database_mr$club_name_home,
        opponent=database_mr$club_name_away,
        home=1),
      tibble(
        goals=database_mr$goals_team_away,
        team=database_mr$club_name_away,
        opponent=database_mr$club_name_home,
        home=0)) %>%
    
    MASS::glm.nb(goals ~ home + team +opponent,data=.)
  summary(nbinom_model)
  
  
  sim <- f_simulate_score_prob(foot_model = nbinom_model,
                               homeTeam = missinggames$club_name_home,
                               awayTeam = missinggames$club_name_away
  ) 
  nbinom <- tibble(all_goals = c(sim$home, sim$away))  
  return(nbinom)
}

#poisson plot
poisson_plot<- function(sim_years){
  database_mr <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds")))%>% filter(between(matchday, 1, 20))
  
  database_season <- read_rds(here::here(paste0("/data/database_season_",sim_years,".rds")))
  
  missinggames <- read_rds(here::here(paste0("/data/database_match_results_",sim_years,".rds"))) %>% .[1:4]
  
  


  
  
  home_goals_avg <- database_mr$goals_team_home %>% mean()
  
  away_goals_avg <- database_mr$goals_team_away %>% mean()
  
  
  poisson_model <- 
    bind_rows(
      tibble(
        goals = database_mr$goals_team_home,
        team = database_mr$club_name_home,
        opponent=database_mr$club_name_away,
        home=1),
      tibble(
        goals=database_mr$goals_team_away,
        team=database_mr$club_name_away,
        opponent=database_mr$club_name_home,
        home=0)) %>%
    
    glm(goals ~ home + team +opponent, family=poisson(link=log),data=.)
  summary(poisson_model)
  
  
  sim <- f_simulate_score_prob(foot_model = poisson_model,
                               homeTeam = missinggames$club_name_home,
                               awayTeam = missinggames$club_name_away
  ) 
  poisson <- tibble(all_goals = c(sim$home, sim$away))  
  return(poisson)
}




