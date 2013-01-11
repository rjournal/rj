update_status <- function(id, status, date = Sys.Date(), comments = "",
                          index = NULL) {
  index <- as.index(index)

  if (is.character(date)) date <- as.Date(date)

  if (!(status %in% valid_status)) {
    stop(status, " is not a known status. Did you mean ",
      amatch_status(status), "?", call. = FALSE)
  }

  article <- find_article(id, index)
  article$status <- c(article$status, status(status, date, comments))
  index <- update_article(article, index)
  save_index(index)
}
