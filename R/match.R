#' Extract keywords from a submitted article
#' @param id the article id
#'
#' @return a vector of the article keywords
get_article_keywords <- function(id){
  # ASK: need to know what the article object looks like
  # ASK: need to know what would trigger this keyword matching step
  # this function is supposed to output a vector of keywords for a given article
  article <- as.article(id)
  article$keywords

}

#' Extract keywords from reviewer list
#' @return a tibble of all the available reviewers
get_reviewer_keywords <- function(){

  sheet_raw <- suppressMessages(googlesheets4::read_sheet("reviewer_sheet"))
  reviewer_info <- tibble::tibble(
    email = sheet_raw$`Email address`,
    gname = sheet_raw$`What's your given name, eg how you would like to be addressed (eg Mike)?`,
    fname = sheet_raw$`What's your full name (eg Michael Kane)?`,
    keywords = sheet_raw$`Please indicate your areas of expertise, check as many as you feel are appropriate.  (Based on available CRAN Task Views.)`
  ) %>%
    tidyr::separate_rows(keywords, sep = ",")

  reviewer_info
}


#' Match reviewers for a submitted article based on keywords
#' @param article_kw vector; keywords of a submitted article
#' @return a tibble of potential reviewers for the article
match_keywords <- function(article_kw){
  reviewer_kw <- get_reviewer_keywords()

  matched <- map(article_kw,
                 ~get_reviewer_keywords() %>%
                   dplyr::filter(keywords == .x)) %>%
                   dplyr::bind_rows()

  matched %>%
    dplyr::distinct(email, .keep_all = TRUE)

}





