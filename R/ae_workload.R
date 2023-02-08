#' Summarise the reviewer's agree/decline ratio based on past invites
#'
#' The function pulls out the agree/decline incidence of all the reviewers
#' based on past invite and calculate the agree percentage of each reviewer.
#' Use \code{tabulate_articles} first to tabulate all the articles in a particular directory
#' and then apply this function.
#'
#' @param articles a tibble summary of articles in the accepted and submissions folder. Output of \code{tabulate_articles()}
#' @param push whether the reviewer number of review completed by the reviewer should be pushed to the reviewer sheet
#' @importFrom tidyr separate_rows pivot_wider
#' @importFrom stringr str_detect word
#' @importFrom scales label_percent
#' @examples
#' \dontrun{
#' articles <- tabulate_articles()
#' reviewer_summary(articles)
#' }
#' @export
reviewer_summary <- function(articles, push = FALSE){
  res <- articles %>%
    dplyr::select(.data$id, .data$reviewers) %>%
    tidyr::unnest(.data$reviewers) %>%
    tidyr::separate_rows(comment, sep = "; ") %>%
    dplyr::filter(stringr::str_detect(comment, "Agreed|Declined")) %>%
    dplyr::mutate(comment = tolower(stringr::word(comment))) %>%
    dplyr::group_by(.data$name, comment) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = comment, values_from = .data$n, values_fill = 0) %>%
    dplyr::relocate(.data$name, .data$agreed, .data$declined) %>%
    dplyr::mutate(ratio = .data$agreed / (.data$agreed + .data$declined),
                  ratio = scales::label_percent()(.data$ratio))

  if (push){
    sheet_raw <- read_reviewer_sheet()
    out <- sheet_raw %>%
      dplyr::left_join(res %>% dplyr::select(.data$name, .data$agreed), by = c("fname" = "name")) %>%
      dplyr::select(.data$agreed)
    range <- paste0("I1:I", nrow(sheet_raw))
    googlesheets4::range_write(reviewer_sheet_url, out, range = range)
  }

  res
}

#' Check the number of articles an AE is currently working on
#'
#' This will examine the DESCRIPTION files for articles in
#' the Submissions folder, check articles that have status
#' "with AE".
#'
#' @param articles a tibble summary of articles in the accepted and submissions
#'   folder. Output of \code{tabulate_articles()}
#' @param day_back numeric; positive number of day to go back for calculating AE
#'   workload. Retains any article where any status entry for an article is
#'   newer than `day_back` days ago.
#'
#' @importFrom dplyr select count left_join right_join filter distinct rename bind_rows
#' @importFrom tidyr unnest replace_na
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' ae_workload()
#' }
#' @export
ae_workload <- function(articles = NULL, day_back = 365) {
  ae_rj <- read.csv(system.file("associate-editors.csv", package = "rj")) %>%
    select(.data$name, .data$initials, .data$email, .data$comment) %>%
    as_tibble() %>%
    rename(status = .data$comment)

  # if don't supply articles, use documented(!) source
  if (is.null(articles)) {
    articles <- tabulate_articles()
  }

  # throw away most of the columns & then unnest status
  articles <- articles %>%
    select(.data$id, .data$ae, .data$status) %>%
    unnest(status)

  # filter articles if day back is provided
  # allow this to be NULL but check if is a numeric if supplied
  articles <- articles %>%
    filter(date >= Sys.Date() - day_back)

  # take only those with "with_AE" status & return rows
  # after this we don't need comments or status
  assignments <- articles %>%
    filter(.data$status == "with AE", .data$ae != "") %>%
    select(-c(.data$comments, status)) %>%
    distinct(id, .keep_all = TRUE)

  # some people use names, other initials for AEs
  # this finds those with only initials and replace the ae with the full name
  tmp <- assignments %>%
    filter(str_length(.data$ae) < 4) %>%
    left_join(ae_rj, by = c("ae" = "initials")) %>%
    select(.data$id, .data$name, .data$date, .data$status) %>%
    rename(ae = .data$name)

  # ... which allows us to take all those with full names...
  assignments %>%
    filter(str_length(.data$ae) >= 4) %>%
    bind_rows(tmp) %>% #... bind on those that had initials
    count(.data$ae, sort = TRUE) %>% # count the assignments by AE
    right_join(ae_rj, by = c("ae" = "name")) %>% # add some some useful info
    replace_na(list(n=0))
}

#' @rdname ae_workload
#' @param x a single article, i.e. as.article("Submissions/2020-114")
#' @examples
#' \dontrun{
#' art <- as.article("Submissions/2020-114")
#' get_AE(art)
#' }
#' @export
get_AE <- function(x){
  list(id = format(x$id), ae = x$ae)
}


#' Add AE to the DESCRIPTION
#'
#' Fuzzy match to find the initial of the AE to fill in the article DESCRIPTION.
#' Checks that AE term has not ended.
#' The status field is also updated with a new line of add AE.
#'
#' @param article article id
#' @param name a name used to match AE, can be AE initials, name, github handle, or email
#' @param date the date for updating status
#' @export


add_ae <-function (article, name, date = Sys.Date()) {
  article <- as.article(article)
  ae_list <- filter(read.csv(system.file("associate-editors.csv",
                                         package = "rj")), .data$end_year > as.numeric(substr(Sys.Date(),
                                                                                         1, 4)))
  found <- NA
  found <- which(str_detect(ae_list$initials, name))
  if (is.na(found))
    found <- which(str_detect(ae_list$name, name))
  if (is.na(found))
    found <- which(str_detect(ae_list$github, name))
  if (is.na(found))
    found <- which(str_detect(ae_list$email, name))
  if (!is.na(found)) {
    article$ae <- ae_list$initials[found]
    update_status(article, "with AE", comments = ae_list$name[found],
                  date = date)
  }
  else {
    cli::cli_alert_warning("No AE found. Input the name as the whole or part of the AE name, github handle, or email")
  }
  return(invisible(article))
}


#' Extract corresponding author from an article
#' @param article Article id, like \code{"2014-01"}
#' @examples
#' \dontrun{
#' # extract from a single article
#' corr_author("Submissions/2020-114")
#'
#' # extract corresponding authors from the active articles
#' all <- active_articles()
#' purrr::map_dfr(all, corr_author)
#' }
#' @importFrom purrr pluck map
#' @importFrom tibble tibble
#' @export
corr_author <- function(article){

  article <- as.article(article)

  all_authors <- article$authors
  # find the index of the author that provide the email
  email <- purrr::map(1:length(all_authors), ~purrr::pluck(all_authors, .x)$email)
  idx <- which(purrr::map_lgl(email, ~!is_null(.x)))

  tibble::tibble(
    corr_author = purrr::pluck(all_authors, idx)$name,
    email = purrr::pluck(all_authors, idx)$email
  )

}
