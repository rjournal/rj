#' Find late reviewers for submissions being handled by a given editor
#'
#' This should be run regularly and reviewers chased up if they are late.
#'
#' @param editor The editor or associate editor handling the submissions
#'
#' @return A data frame of reviewers who have not responded or not submitted their review on time.
#' @details The number of stars has the following meaning:
#' \itemize{
#'   \item 1 star: not responded in more than a week or accepted more than 4 weeks ago;
#'   \item 2 stars: not responded in more than 2 weeks or accepted more than 8 weeks ago;
#'   \item 3 stars: not responded in more than 3 weeks or accepted more than 12 weeks ago;
#' }
#' Please chase up late reviewers.
#' If a reviewer has declined, use \code{\link{decline_reviewer}} to remove them from the list.
#' If you have decided to abandon a reviewer, use \code{\link{abandon_reviewer}}.
#' @export

late_reviewers <- function(editor) {
  articles <- get_assignments(editor) |>
    get_latest()
  articles <- articles[articles$status == "out for review", ]
  if (NROW(articles) == 0L) {
    warning("No articles are out for review")
    return(NULL)
  }
  reviewers <- articles[c("id", "reviewers")] |>
    tidyr::unnest(reviewers)
  invited <- stringr::str_detect(reviewers$comment, ".*Invited [0-9]*\\-[0-9]*\\-[0-9]*$") &
    !is.na(reviewers$comment)
  agreed <- stringr::str_detect(reviewers$comment, ".*Agreed [0-9]*\\-[0-9]*\\-[0-9]*$") &
    !is.na(reviewers$comment)
  output <- dplyr::arrange(reviewers[invited | agreed, ], comment)
  agreed <- stringr::str_detect(output$comment, ".*Agreed [0-9]*\\-[0-9]*\\-[0-9]*$")
  dates <- stringr::str_extract(output$comment, "[0-9]*\\-[0-9]*\\-[0-9]*$") |>
    as.Date()
  days <- Sys.Date() - dates
  agreed_stars <- unlist(lapply(days, function(u) sum(u > c(4L, 8L, 12L) * 7)))
  invited_stars <- unlist(lapply(days, function(u) sum(u > c(7L, 14L, 21L))))
  nstars <- (invited_stars * !agreed) + (agreed_stars * agreed)
  output$stars <- stringr::str_dup("*", nstars)
  output <- as.data.frame(dplyr::arrange(output, -nstars))
  output$status <- stringr::str_extract(output$comment, "[a-zA-Z\\s]*[Agreed|Invited] [0-9]*\\-[0-9]*\\-[0-9]*$")
  output[nstars > 0, c("id", "name", "status", "stars")]
}

#' Find late AEs for submissions being handled by a given editor
#'
#' This should be run regularly and AEs chased up if they are late.
#'
#' @param editor The editor handling the submissions
#'
#' @return A data frame of AEs who are late in handling a paper.
#' @details The number of stars has the following meaning:
#' \itemize{
#'   \item 1 star: not responded in more than 12 weeks;
#'   \item 2 stars: not responded in more than 18 weeks;
#'   \item 3 stars: not responded in more than 24 weeks;
#' }
#' Please chase up late AEs.
#' @export

late_aes <- function(editor) {
  articles <- get_assignments(editor) |>
    get_latest()
  articles <- articles[articles$status == "with AE", ]
  if (NROW(articles) == 0L) {
    warning("No articles are with AEs")
    return(NULL)
  }
  days <- Sys.Date() - as.Date(articles$date)
  nstars <- unlist(lapply(days, function(u) sum(u > c(12L, 18L, 24L) * 7)))
  articles$stars <- stringr::str_dup("*", nstars)
  output <- dplyr::arrange(articles, date) |> as.data.frame()
  rownames(output) <- output$id
  output[output$stars != "", c("date", "ae", "stars")]
}

