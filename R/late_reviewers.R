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
  articles <- articles[articles$status == "out for review",]
  if(NROW(articles) == 0L) {
    warning("No articles are out for review")
    return(NULL)
  }
  reviewers <- articles[c("id", "reviewers")] |>
    tidyr::unnest(reviewers)
  invited <- stringr::str_detect(reviewers$comment,".*Invited [0-9]*\\-[0-9]*\\-[0-9]*$") &
    !is.na(reviewers$comment)
  agreed <- stringr::str_detect(reviewers$comment,".*Agreed [0-9]*\\-[0-9]*\\-[0-9]*$") &
    !is.na(reviewers$comment)
  output <- dplyr::arrange(reviewers[invited | agreed,], comment)
  invited <- invited[invited | agreed]
  dates <- stringr::str_extract(output$comment, "[0-9]*\\-[0-9]*\\-[0-9]*$") |>
    as.Date()
  days <- Sys.Date() - dates
  agreed_stars <- unlist(lapply(days, function(u) sum(u > c(4L, 8L, 12L) * 7)))
  invited_stars <- unlist(lapply(days, function(u) sum(u > c(7L, 14L, 21L))))
  stars <- invited_stars * invited + agreed_stars * !invited
  output$stars <- stringr::str_dup("*", stars)
  return(output[stars > 0,])
}
