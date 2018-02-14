#' Update the status of an article.
#'
#' \code{reject}, \code{accept} and \code{withdraw} update the status,
#' move the file to the correct directory and draft an email from a 
#' template of the corresponding name.
#'
#' @rdname action
#' @export
#' @param article as \code{\link{article}} object, path, or ID. See
#'   \code{\link{as.article}} for more details about how article is located.
#' @param status new status to add
#' @param comments any additional comments
#' @param date date of status update. If omitted defaults to today.
update_status <- function(article, status, comments = "", date = Sys.Date()) {
  article <- as.article(article)

  if (is.character(date)) date <- as.Date(date)

  article$status <- c(article$status, status(status, date, comments))
  save_article(article, quiet = TRUE)
}

#' @rdname action
#' @export
reject <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  message("Rejecting ", format(article$id))
  update_status(article, "rejected", comments = comments, date = date)

  system(paste("git mv",
               article$path, file.path("Rejected", basename(article$path))))
  email_template(article, "reject")

  invisible()
}

#' @rdname action
#' @export
accept <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  message("Accepting ", format(article$id))
  update_status(article, "accepted", comments = comments, date = date)

  system(paste("git mv",
               article$path, file.path("Accepted", basename(article$path))))
  email_template(article, "accept")

  invisible()
}

#' @rdname action
#' @export
withdraw <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  message("Withdrawing ", format(article$id))
  update_status(article, "withdrawn", comments = comments, date = date)

  system(paste("git mv",
               article$path, file.path("Rejected", basename(article$path))))
  email_template(article, "widthdraw")

  invisible()
}
