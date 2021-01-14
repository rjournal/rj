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

  from = article$path
  to = file.path("Rejected", basename(article$path))
  msg = paste("Moving", from, "to", to)
  cli::cli_alert_info(msg)
  system2("git", args = c("mv", from, to))

  cli::cli_alert_info("Creating Email")
  email_template(article, "reject")
  cli::cli_alert_info("If your browser doesn't open, check getOption('browser')")

  return(invisible(NULL))
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

  return(invisible(NULL))
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

  return(invisible(NULL))
}

#' @importFrom gmailr mime create_draft
draft_proofing <- function(subs) {
  proof_sub <- function(sub) {
    body <- render_template(sub, "gmail_proof")
    # Note this should be from current editor's address
    email <- gmailr::mime(From = "dicook.rj@@gmail.com",
                          To = sub$authors[[1L]]$email,
                          Subject = paste("R Journal article proofing",
                                          format(sub$id)),
                          body = body)
    gmailr::create_draft(email)
  }
  ans <- lapply(subs, proof_sub)
  names(ans) <- vapply(subs, function(s) format(s$id),
                       FUN.VALUE = character(1L))
  ans
}

#' Send submission acknowledgement drafts
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

  name <- "proofing_request.txt"
  path <- file.path(dest, name)

  data <- as.data(article)
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 5, "%d %b %Y")

  template <- find_template("gmail_proof")
  email <- whisker.render(readLines(template), data)

  writeLines(email, path)

  update_status(data$id, "out for proofing")
  invisible(TRUE)
}
