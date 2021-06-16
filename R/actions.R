#' Update the status of an article.
#'
#' \code{reject}, \code{accept} and \code{withdraw} update the status,
#' move the file to the correct directory and draft an email from a
#' template of the corresponding name. Use \code{valid_status} to check
#' for possible statuses.
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
  save_article(article)
}

#' @rdname action
#' @export
reject <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)

  cli::cli_h1(paste("Rejecting paper", format(article$id)))
  cli::cli_alert_info("Updating DESCRIPTION file")
  update_status(article, "rejected", comments = comments, date = date)

  apath <- get_articles_path()
  from <- article$path
  to <- file.path(apath, "Rejected", basename(article$path))
  msg <- paste("Moving", from, "to", to)
  cli::cli_alert_info(msg)
  git("mv", from, to)

  cli::cli_alert_info("Creating Email")
  email_template(article, "reject")
  cli::cli_alert_info("If your browser doesn't open, check getOption('browser')")

  return(invisible(NULL))
}

#' @rdname action
#' @export
reject_format <- function(article, comments = "", date = Sys.Date()) {
  #article <- as.article(article)

  data <- as.data(as.article(article))
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 5, "%d %b %Y")

  cli::cli_h1(paste("Rejecting paper", format(data$id)))
  cli::cli_alert_info("Updating DESCRIPTION file")
  update_status(article, "rejected", comments = comments, date = data$date)

  apath <- get_articles_path()
  from <- data$path
  to <- file.path(apath, "Rejected", basename(data$path))
  msg <- paste("Moving", from, "to", to)
  cli::cli_alert_info(msg)
  git("mv", from, to)

  #cli::cli_alert_info("Creating Email")
  #email_template(article, "reject_format")
  #cli::cli_alert_info("If your browser doesn't open, check getOption('browser')")

  template <- find_template("reject_format")
  email <- whisker.render(readLines(template), data)
  email_text(email)
  cli::cli_alert_info("If your browser doesn't open, check getOption('browser') and set options(browser=Sys.getenv('R_BROWSER'))")

  return(invisible(NULL))
}

#' @rdname action
#' @export
accept <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  message("Accepting ", format(article$id))
  update_status(article, "accepted", comments = comments, date = date)

  apath <- get_articles_path()
  git("mv", article$path,
            file.path(apath, "Accepted", basename(article$path)))
  email_template(article, "accept")

  return(invisible(NULL))
}

#' @rdname action
#' @export
withdraw <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  message("Withdrawing ", format(article$id))
  update_status(article, "withdrawn", comments = comments, date = date)

  apath <- get_articles_path()
  git("mv", article$path,
            file.path(apath, "Rejected", basename(article$path)))
  email_template(article, "widthdraw")

  return(invisible(NULL))
}

#' Get list of articles in the Accepted folder to be proofed
#' This can be used with \code{draft_proofing} to construct
#' emails to authors on the final version.
#'
#' @export
get_accepted_articles <- function() {
  # Warning: this gets all the articles that have been accepted
  # If any are not to appear in the issue need to work out how to ignore
  apath <- get_articles_path()
  acc <- list.files(file.path(apath, "Accepted"))

  cli::cat_line("Drafting proofing emails")
  draft_proofing(acc)

  invisible(NULL)
}

#' Generate proofing email for one article
#'
#' @param article this is the article id
#'
#' @export
draft_proofing <- function(article) {
  data <- as.data(as.article(article))
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 5, "%d %b %Y")

  template <- find_template("gmail_proofing")
  email <- whisker.render(readLines(template), data)

  email_text(email)
}

#draft_proofing <- function(accepted) {
#  proof_sub <- function(acc) {
#    body <- render_template(acc, "gmail_proofing")
#    acc_meta <- as.data(as.article(acc))
    # Note this should be from current editor's address
#    email <- gmailr::mime(From = "dicook.rj@@gmail.com",
#                          To = acc_meta$email,
#                          Subject = paste("R Journal article proofing",
#                                          format(acc_meta$id)),
#                          body = body)
#    gmailr::create_draft(email)
#
#
#  }
#  ans <- lapply(accepted, proof_sub)
#  names(ans) <- vapply(accepted, function(s) format(s$id),
#                       FUN.VALUE = character(1L))
#  ans
#}

#' Send proofing article emails
#'
#' @param drafts list of \code{gmail_draft} objects
#' @importFrom gmailr send_draft
#' @export
proofing_article <- function(drafts) {
  for (draft in drafts) {
    gmailr::send_draft(draft)
  }
  for (id in names(drafts)) {
    update_status(id, "out for proofing")
  }
  invisible(TRUE)
}

#' This function writes the email text into the correspondence folder
#'
#' @param article article id
#' @export
proofing_article_text <- function(article) {
  article <- as.article(article)

  dest <- file.path(article$path, "correspondence")
  if (!file.exists(dest)) dir.create(dest)

  filename <- "proofing_request.txt"
  path <- file.path(dest, filename)

  data <- as.data(article)
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 5, "%d %b %Y")

  template <- find_template("gmail_proofing")
  email <- whisker.render(readLines(template), data)

  writeLines(email, path)

  update_status(data$id, "out for proofing")
  invisible(TRUE)

}
