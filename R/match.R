#' Extract keywords from a submitted article
#' @param id the article id
#'
#' @return a vector of the article keywords
#' @export
get_article_keywords <- function(id){
  article <- as.article(id)
  keywords_raw <- article$keywords
  author <- unlist(article$author)
  if (nchar(keywords_raw) == 0) {
    rlang::abort("no keyword detected in the DESCRIPTION file!")
  }

  list(keywords = as.vector(stringr::str_split(keywords_raw, ", ", simplify = TRUE)),
       author = author[names(author) == "name"] )

}

#' Extract keywords from reviewer list
#' @return a tibble of all the available reviewers
get_reviewer_keywords <- function(){

  sheet_raw <- suppressMessages(googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1stC58tDHHzjhf63f7PhgfiHJTJkorvAQGgzdYL5NTUQ/edit?ts=606a86e4#gid=1594007907"))
  reviewer_info <- tibble::tibble(
    email = sheet_raw$`Email address`,
    gname = sheet_raw$`What's your given name, eg how you would like to be addressed (eg Mike)?`,
    fname = sheet_raw$`What's your full name (eg Michael Kane)?`,
    keywords = sheet_raw$`Please indicate your areas of expertise, check as many as you feel are appropriate.  (Based on available CRAN Task Views.)`
  ) %>%
    tidyr::separate_rows(.data$keywords, sep = ",")

  reviewer_info
}


#' Match reviewers for a submitted article based on keywords
#' @param id the article id in the description file
#' @param n numeric; number of reviewer to display
#' @importFrom rlang .data
#' @return a tibble of potential reviewers for the article
#' @export
match_keywords <- function(id, n = 5){

  article <- get_article_keywords(id)
  article_kw <- article$keywords
  ae <- AEs()

  reviewer_kw <- get_reviewer_keywords() %>%
    filter(!.data$fname %in% article$author,
           !.data$email %in% ae$email)

  article_kw_standardised <- keywords_list_concat %>%
    dplyr::filter(.data$submission %in% article_kw) %>%
    dplyr::pull(.data$reviewer_topics)

  matched <- map(article_kw_standardised,
                 ~reviewer_kw %>%
                   dplyr::filter(.data$keywords == !!.x)) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(.data$fname) %>%
    dplyr::tally(sort = TRUE)

  matched_count <- matched %>%
    dplyr::group_by(n) %>%
    dplyr::tally(name = "count") %>%
    dplyr::arrange(dplyr::row_number())

  i <- 1
  out <- vector()
  while (length(out) == 0){
    if (sum(matched_count$count[i:nrow(matched_count)]) == n){
      threshold_in <- matched_count$n[i]
      inform(glue::glue("{n} reviewers with {threshold_in} matches"))
      out <- matched %>% dplyr::filter(n >= threshold_in) %>% dplyr::pull(.data$fname)

    } else if (sum(matched_count$count[i:nrow(matched_count)]) < n){
      threshold_in <- matched_count$n[i]
      threshold_out <- matched_count$n[i-1]
      out1 <- matched %>% dplyr::filter(n >= threshold_in) %>% dplyr::pull(.data$fname)
      pool <- matched %>% dplyr::filter(n == threshold_out) %>% dplyr::pull(.data$fname)
      size <- n - length(out1)
      out2 <- pool %>% sample(size)
      inform(glue::glue("first {length(out1)} reviewers with {threshold_in} matches; next {size} reviewers with {threshold_out} matches"))
      out <- c(out1, out2)
    }else if (i == nrow(matched_count) & sum(matched_count$count[i:nrow(matched_count)]) > n){
      pool <- matched %>% dplyr::filter(n == i) %>% dplyr::pull(.data$fname)
      inform(glue::glue("{n} reviewers with {i} matches"))
      out <- pool %>% sample(n)
    }

    i <- i + 1
  }

  reviewer_kw %>%
    dplyr::filter(.data$fname %in% out) %>%
    dplyr::select(.data$fname, .data$gname, .data$email) %>%
    dplyr::distinct() %>%
    dplyr::arrange(factor(.data$fname, levels = out))


}

