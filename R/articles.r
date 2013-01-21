#' Return a list of all active articles
#'
#' Looks in \file{Submissions/}.
#' @export
active_articles <- function() {
  base <- c("Submissions")
  paths <- dir(base, full.names = TRUE)
  lapply(paths, as.article)
}

