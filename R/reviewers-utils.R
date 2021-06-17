add_reviewer_comment <- function(article, reviewer_id, comment) {
  article <- as.article(article)
  reviewers <- article$reviewers
  reviewer <- reviewers[[reviewer_id]]
  old_comment <- reviewer$comment
  if (!is.null(old_comment) && nchar(old_comment) > 0) {
    old_comment <- stringr::str_remove(old_comment, "\\[|\\]")
    comment <- paste0(old_comment, "; ", comment)
  }
  reviewer$comment <- comment
  article$reviewers[[reviewer_id]] <- reviewer
  cli::cli_alert_info(comment)
  article <- save_article(article)
  return(invisible(article))
}

check_dup_comment <- function(article, reviewer_id, test_string) {
  article <- as.article(article)
  comment <- article$reviewers[[reviewer_id]]$comment
  check <- stringr::str_detect(comment, pattern = test_string)
  if (isTRUE(check)) {
    stop(paste("Reviewer", reviewer_id, "has already", test_string), call. = FALSE)
  }
  return(invisible(NULL))
}
