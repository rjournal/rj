active_articles <- function() {
  base <- c("Submissions")
  paths <- dir(base, full.names = TRUE)
  lapply(paths, as.article)
}

