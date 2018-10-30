#' Functions for scraping data
#'
#'
#' @param base_url url provided by user
#'
#' @export
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import stringr


### FUNCTION: GET N PAGES ###

n_pages <- function(base_url){

  last_page <- html               %>%
      html_nodes(".review-pager") %>%
      html_text()                 %>%
      str_split("1")              %>%
      map(2)                      %>%
      unlist()                    %>%
      parse_number()
}

### FUNCTION: GET ALL PAGES ###

get_pages <- function(base_url){

  last_page    = n_pages(base_url)
  page_numbers = c(1,seq_along(2:last_page) * 20)
  urls         = paste0(base_url,"?start=", page_numbers)
}

### FUNCTION: GET ID ###

get_id <- function(html) {

  id <- html          %>%
    html_nodes("div") %>%
    html_nodes(xpath = '//meta[@itemprop="author"]')  %>%
    html_attr('content')
}

### FUNCTION: GET DATE ###

get_date <- function(html) {

  date <- html        %>%
    html_nodes("div") %>%
    html_nodes(xpath = '//meta[@itemprop="datePublished"]') %>%
    html_attr('content')
}

### FUNCTION: GET RATING ###

get_rating <- function(html) {

  rating <- html      %>%
    html_nodes("div") %>%
    html_nodes(xpath = '//meta[@itemprop="ratingValue"]') %>%
    html_attr('content') %>%
    as.numeric() %>%
    .[-1]
}

### FUNCTION: GET REVIEW ###

get_review <- function(html) {

  review <- html      %>%
    html_nodes("div") %>%
    html_nodes(xpath = '//p[@itemprop="description"]') %>%
    html_text()         %>%
    str_remove("\n")
}
