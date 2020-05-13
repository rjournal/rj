#' List articles.
#'
#' List all articles in common directories.
#'
#' \itemize{
#'  \item \code{active_articles}: \file{Submissions/}, \file{Accepted/}
#'  \item \code{accepted_articles}: \file{Accepted/}
#' }
#' @export
active_articles <- function() {
  base <- c("Submissions", "Accepted")
  paths <- dir(base, full.names = TRUE)
  lapply(paths, as.article)
}

#' @rdname active_articles
#' @export
accepted_articles <- function() {
  paths <- dir("Accepted", full.names = TRUE)
  lapply(paths, as.article)
}

#' Generate a new id value.
#'
#' Inspects submissions/, accepted/ and rejected to figure out which
#' id is next in sequence.
#'
#' @export
new_id <- function() {
  ids <- dir(file.path(get_articles_path(), c("Submissions", "Accepted", "Rejected")))
  ids <- lapply(ids, parse_id)

  this_year <- Filter(function(x) x$year == year(), ids)

  if (length(this_year) == 0) {
    id(year(), 1)
  } else {
    seqs <- vapply(this_year, function(x) x$seq, integer(1))
    id(year(), max(seqs) + 1L)
  }
}

#' Find articles with a given status.
#' @param articles A vector of articles
#' @param status The status you are looking for
filter_status <- function(articles, status) {
    Filter(function(a) last_status(a)$status == status, articles)
}
