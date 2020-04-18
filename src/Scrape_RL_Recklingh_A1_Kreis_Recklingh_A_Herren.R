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
