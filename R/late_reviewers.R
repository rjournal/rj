#' Find late reviewers for submissions being handled by a given editor
#'
#' This should be run regularly and reviewers chased up if they are late.
#'
#' @param editor The editor or associate editor handling the submissions
#'
#' @return A data frame of reviewers who have not submitted their review on time.
#'   1 star: invited more than 6 weeks ago;
#'   2 stars: invited more than 12 weeks ago;
#'   3 stars: invited more than 18 weeks ago.
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
  active <- stringr::str_detect(reviewers$comment,".*Invited [0-9]*\\-[0-9]*\\-[0-9]*$") &
    !is.na(reviewers$comment)
  output <- dplyr::arrange(reviewers[active,], comment)
  date_invited <- stringr::str_extract(output$comment, "[0-9]*\\-[0-9]*\\-[0-9]*") |>
    as.Date()
  days <- Sys.Date() - date_invited
  deadlines <- c(6L, 12L, 18L) * 7
  stars <- unlist(lapply(days, function(u) sum(u > deadlines)))
  output$stars <- stringr::str_dup("*", stars)
  return(output[stars > 0,])
}
