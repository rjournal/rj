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
    return(invisible(NULL))
  }
  # Add stars
  days <- Sys.Date() - as.Date(articles$date)
  nstars <- unlist(lapply(days, function(u) sum(u > c(12L, 18L, 24L) * 7)))
  articles$stars <- stringr::str_dup("*", nstars)
  output <- dplyr::arrange(articles, date) |> as.data.frame()
  # Replace AE initials with name
  aes <- AEs()
  output$ae <- aes[match(output$ae, aes$initials), "name"]
  # Return
  output <- output[output$stars != "", c("id", "date", "ae", "stars")]
  if(NROW(output) == 0L) {
    return(invisible(NULL))
  } else {
    return(output)
  }
}

#' Find all reviewers for submissions being handled by a given editor
#'
#' @param editor The editor or associate editor handling the submissions
#'
#' @return A data frame of reviewers
#' @export

get_reviewers <- function(editor) {
  articles <- get_assignments(editor) |>
    get_latest()
  articles <- articles[articles$status == "out for review", ]
  if (NROW(articles) == 0L) {
    warning("No articles are out for review")
    return(invisible(NULL))
  } else {
    articles[c("id", "reviewers")] |>
      tidyr::unnest(reviewers)
  }
}

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
  reviewers <- get_reviewers(editor)
  if(is.null(reviewers)) {
    return(invisible(NULL))
  }
  status <- last_reviewer_status(reviewers$comment)
  invited <- status == "Invited" & !is.na(reviewers$comment)
  agreed <- status == "Agreed" & !is.na(reviewers$comment)
  output <- dplyr::arrange(reviewers[invited | agreed, ], comment)
  agreed <- last_reviewer_status(output$comment) == "Agreed"
  dates <- stringr::str_extract(output$comment, "[0-9]*\\-[0-9]*\\-[0-9]*$") |>
    as.Date()
  days <- Sys.Date() - dates
  agreed_stars <- unlist(lapply(days, function(u) sum(u > c(4L, 8L, 12L) * 7)))
  invited_stars <- unlist(lapply(days, function(u) sum(u > c(7L, 14L, 21L))))
  nstars <- (invited_stars * !agreed) + (agreed_stars * agreed)
  output$stars <- stringr::str_dup("*", nstars)
  output <- as.data.frame(dplyr::arrange(output, -nstars))
  output$status <- stringr::str_extract(output$comment, "[a-zA-Z\\s]*[Agreed|Invited] [0-9]*\\-[0-9]*\\-[0-9]*$")
  output <- output[output$stars != "", c("id", "name", "status", "stars")]
  if(NROW(output) == 0L) {
    return(invisible(NULL))
  } else {
    return(output)
  }
}

#' Find articles that need reviewers for submissions being handled by a given editor.
#'
#' Returns all articles with fewer than 2 invited reviewers.
#' This should be run regularly and new reviewers appointed if necessary.
#'
#' @param editor The editor or associate editor handling the submissions
#'
#' @return A data frame of papers needing more reviewers
#' @export

need_reviewers <- function(editor) {
  reviewers <- get_reviewers(editor)
  if(is.null(reviewers)) {
    return(invisible(NULL))
  }
  reviewers <- dplyr::filter(reviewers, !is.na(reviewers$comment))
  # Extract last status
  status <- last_reviewer_status(reviewers$comment)
  output <- reviewers[!status %in% c("Declined","Abandoned"),] |>
    dplyr::select(id) |>
    dplyr::count(id) |>
    dplyr::filter(n < 2)
  output <- as.data.frame(dplyr::arrange(output, n))
  colnames(output) <- c("id", "number_reviewers")
  if(NROW(output) == 0L) {
    return(invisible(NULL))
  } else {
    return(output)
  }
}

# Extract last reviewer status from string
last_reviewer_status <- function(string) {
  # Remove last date
  status <- stringr::str_remove(string, " [0-9]*\\-[0-9]*\\-[0-9]*$")
  # Extract last status
  stringr::str_extract(status, "[a-zA-Z]*$")
}
