#' Extract keywords from a submitted article
#' @param id the article id
#'
#' @return a vector of the article keywords
#' @export
get_article_keywords <- function(id){
  # ASK: need to know what the article object looks like
  # ASK: need to know what would trigger this keyword matching step
  # this function is supposed to output a vector of keywords for a given article
  keywords_raw <- as.article(id)$keywords
  if (nchar(keywords_raw) == 0) {
    rlang::abort("no keyword detected in the DESCRIPTION file!")
  }

  as.vector(stringr::str_split(keywords_raw, ", ", simplify = TRUE))

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
    tidyr::separate_rows(keywords, sep = ",")

  reviewer_info
}


#kw <- c("Graphics", "Official Statistics", "Social Sciences")

#kw <- c("Graphics and Visualisation","Multivariate Statistics")
#' Match reviewers for a submitted article based on keywords
#' @param article_kw vector; keywords of a submitted article
#' @param n numeric; number of reviewer to display
#' @return a tibble of potential reviewers for the article
#' @export
match_keywords <- function(article_kw, n = 5){
  reviewer_kw <- get_reviewer_keywords()

  article_kw_standardised <- keywords_list_concat %>%
    dplyr::filter(submission %in% article_kw) %>%
    dplyr::pull(reviewer_topics)

  matched <- map(article_kw_standardised,
                 ~reviewer_kw %>%
                   dplyr::filter(keywords == !!.x)) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(fname) %>%
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
      out <- matched %>% dplyr::filter(n >= threshold_in) %>% dplyr::pull(fname)

    } else if (sum(matched_count$count[i:nrow(matched_count)]) < n){
      threshold_in <- matched_count$n[i]
      threshold_out <- matched_count$n[i-1]
      out1 <- matched %>% dplyr::filter(n >= threshold_in) %>% dplyr::pull(fname)
      pool <- matched %>% dplyr::filter(n == threshold_out) %>% dplyr::pull(fname)
      size <- n - length(out1)
      out2 <- pool %>% sample(size)
      inform(glue("randomly select {size} reviewers from {length(pool)} reviewers with {threshold_out} matches"))
      out <- c(out1, out2)
    }else if (i == nrow(matched_count) & sum(matched_count$count[i:nrow(matched_count)]) > n){
      pool <- matched %>% dplyr::filter(n == i) %>% dplyr::pull(fname)
      inform(glue::glue("randomly select {n} reviewers from {length(pool)} reviewers with {i} matches"))
      out <- pool %>% sample(n)
    }

    i <- i + 1
  }

  reviewer_kw %>%
    dplyr::filter(fname %in% out) %>%
    dplyr::select(fname, gname, email) %>%
    dplyr::distinct() %>%
    dplyr::arrange(factor(fname, levels = out))


}

# it seems that the keywords from the reviewer is not the set of keywords for submission

# how many keywords matches
# 2 reviewers per article



