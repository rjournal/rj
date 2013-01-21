#' Return a list of all active articles
#'
#' Looks in \file{Submissions/}.
#' @export
active_articles <- function() {
  base <- c("Submissions")
  paths <- dir(base, full.names = TRUE)
  lapply(paths, as.article)
}

#' Generate a new id value.
#'
#' @export
new_id <- function() {
  ids <- dir(c("Submissions", "Accepted", "Rejected"))
  ids <- lapply(ids, parse_id)

  this_year <- Filter(function(x) x$year == year(), ids)

  if (length(this_year) == 0) {
    id(year(), 1)
  } else {
    seqs <- vapply(this_year, function(x) x$seq, integer(1))
    id(year(), max(seqs) + 1L)
  }
}
