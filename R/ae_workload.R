#' Tabulate reviewer's Accept/Decline incidence
#'
#' The function examine the DESCRIPTION files of articles tabulated by
#' \code{tabulate_articles} and count the number of accept and decline
#' of each reviewer.
#' @param articles a tibble summary of articles in the accepted and submissions folder. Output of \code{tabulate_articles()}
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom stringr str_detect word
#' @examples
#' \dontrun{
#' articles <- tabulate_articles()
#' reviewer_summary(articles)
#' }
#' @export
reviewer_summary <- function(articles){
  articles %>%
    dplyr::select(id, reviewers) %>%
    tidyr::unnest(reviewers) %>%
    tidyr::separate_rows(comment, sep = "; ") %>%
    dplyr::filter(stringr::str_detect(comment, "Agreed|Accepted|Declined")) %>%
    dplyr::mutate(comment = stringr::word(comment),
           comment = ifelse(comment == "Agreed", "Accepted", comment)) %>%
    dplyr::group_by(name, comment) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = comment, values_from = n)
}
#' Check the number of articles an AE is currently working on
#'
#' This will examine the DESCRIPTION files for articles in
#' the Submissions folder, check articles that have status
#' "with AE".
#' @param articles a tibble summary of articles in the accepted and submissions folder. Output of \code{tabulate_articles()}
#' @param day_back number of day to go back for calculating AE workload.
#' @importFrom dplyr select count left_join
#' @importFrom tidyr unnest
#' @examples
#' \dontrun{
#' articles <- tabulate_articles()
#' ae_workload(articles)
#' }
#' @export
ae_workload <- function(articles, day_back = 365) {

  ae_rj <- read.csv(system.file("associate-editors.csv", package = "rj")) %>%
    select(.data$name, .data$email)

  articles %>%
    dplyr::select(id, status) %>%
    tidyr::unnest(status) %>%
    dplyr::filter(status == "with AE") %>%
    dplyr::rename(ae = comments) %>%
    dplyr::group_by(ae) %>%
    dplyr::filter(date >= Sys.Date()- day_back) %>%
    dplyr::count(ae) %>%
    dplyr::left_join(ae_rj, by = c("ae" = "name"))

}

#' Add AE to the DESCRIPTION
#'
#' Fuzzy match to find the initial of the AE to fill in the article DESCRIPTION.
#' The status field is also updated with a new line of add AE.
#'
#' @param article article id
#' @param name a name used to match AE. Allow fuzzy match of the whole or part of the AE name, github handle, or email
#' @param date the date for updating status
#' @export
add_ae <- function(article, name, date = Sys.Date()){
  article <- as.article(article)

  ae_list <- read.csv(system.file("associate-editors.csv", package = "rj")) %>%
    mutate(concat = paste0(!!sym("name"), !!sym("github_handle"), !!sym("email")))

  person <- ae_list$github[str_detect(ae_list$concat, name)]
  person_name <- as.character(ae_list$name[str_detect(ae_list$concat, name)])

  if (length(person) != 0){
    # github start with "ae-articles-xxx"
    ae_abbr <- str_sub(person, 13, -1)
    article$ae <- ae_abbr
    update_status(article, "with AE", comments = person_name, date = date)

  } else {
    cli::cli_alert_warning("No AE found. Input the name as the whole or part of the AE name, github handle, or email")
  }

  return(invisible(article))
}
