#' Update the article status
#'
#' This is a general function for updating the status field in the DESCRIPTION.
#'
#'
#' @param article Article id, like \code{"2014-01"}
#' @param status new status to add, see details section for more
#' @param comments Any additional comments
#' @param date Date of status update. If omitted defaults to today.
#' @param AE Logical, if \code{TRUE}, \code{"AE: "} is prefixed to the status
#' @param replace logical, if the last status already matches \code{status}
#'        then the status is only updated if this flag is set to \code{TRUE}.
#'
#' @details
#' For AEs, status is prefixed with "AE: " and valid status includes
#' "AE: major revision", "AE: minor revision", "AE: accept", and "AE: reject".
#'
#' For Editors, use \code{accept()}, \code{reject()}, and \code{withdraw()} to
#' update the status as well as draft an email to the correspondence author.
#'
#' Check valid status with \code{valid_status}.
#'@examples
#'\dontrun{
#' update_status("2020-114", status = "AE: major revision")
#'}
#' @export
update_status <- function(article, status, comments = "", date = Sys.Date(), AE = is_AE(), replace=TRUE) {
  article <- as.article(article)

  if (is.character(date)) date <- as.Date(date)

  if (AE && status %in% c("major revision", "minor revision", "accept", "rejected")) {
      cli::cli_abort(
        'AE should use "AE: " prefixed status for major revision,
        minor revision, accept, and rejected.
        See {.code valid_status} for all valid status.')
  }

  if (length(article$status)) {
      last <- article$status[[length(article$status)]]
      if (last$status == status) {
          if (!isTRUE(replace)) return(invisible(article))
          warning("Article ", article$id, " already has last entry ", status, ", replacing it")
          article$status <- status_list(article$status[-length(article$status)])
      }
  }

  article$status <- c(article$status, status(status, date, comments))
  save_article(article)
}

#' Accept, reject, or withdraw an article
#'
#' This set of functions wraps around \code{update_status()} and \code{email_template}
#' to first update the status field in the DESCRIPTION file and
#' then draft an email from the template. Articles are verified to be under the
#' Submission folder before carrying out the actions to avoid mistake
#' input of article ID.
#'
#' @inheritParams update_status
#' @rdname action
#' @export
reject <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  check_in_submission_folder(article)
  cli::cli_h1(paste("Rejecting paper", format(article$id)))
  cli::cli_alert_info("Updating DESCRIPTION file")
  update_status(article, "rejected", comments = comments, date = date)

  apath <- get_articles_path()
  from <- article$path
  to <- file.path(apath, "Rejected", basename(article$path))
  msg <- paste("Moving", from, "to", to)
  cli::cli_alert_info(msg)

  # Make sure folder is already added to git
  git("add", from)
  git("mv", from, to)
  article$path <- to

  cli::cli_alert_info("Creating Email")
  email_template(article, "reject")

  return(invisible(NULL))
}

#' @rdname action
#' @export
reject_format <- function(article, comments = "", date = Sys.Date()) {
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
  data$path <- to

  # cli::cli_alert_info("Creating Email")
  # email_template(article, "reject_format")
  # cli::cli_alert_info("If your browser doesn't open, check getOption('browser')")

  template <- find_template("reject_format")
  email <- whisker.render(readLines(template), data)
  save_email(data, email)
  email_text(email)

  return(invisible(NULL))
}

#' @rdname action
#' @export
accept <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  check_in_submission_folder(article)
  message("Accepting ", format(article$id))
  update_status(article, "accepted", comments = comments, date = date)

  apath <- get_articles_path()
  to <- file.path(apath, "Accepted", basename(article$path))
  git("mv", article$path, to)
  article$path <- to
  email_template(article, "accept")

  return(invisible(NULL))
}

#' @rdname action
#' @export
withdraw <- function(article, comments = "", date = Sys.Date()) {
  article <- as.article(article)
  check_in_submission_folder(article)
  message("Withdrawing ", format(article$id))
  update_status(article, "withdrawn", comments = comments, date = date)

  apath <- get_articles_path()
  to <- file.path(apath, "Rejected", basename(article$path))
  git("mv", article$path, to)
  article$path <- to
  email_template(article, "withdraw")

  return(invisible(NULL))
}

#' @rdname action
#' @export
resubmission <- function(article, comments = "", date = Sys.Date()){
  article <- as.article(article)
  check_in_submission_folder(article)
  cli::cli_inform("Request resubmission: {.field {article$id}}")
  update_status(article, "resubmission", comments = comments, date = date)

  email_template(article, "resubmission")
  return(invisible(NULL))
}

#' @rdname action
#' @export
major_revision <- function(article, comments = "", date = Sys.Date()){
  article <- as.article(article)
  check_in_submission_folder(article)
  cli::cli_inform("Major revision: {.field {article$id}}")
  update_status(article, "major revision", comments = comments, date = date)

  email_template(article, "revision-major")
  return(invisible(NULL))
}

#' @rdname action
#' @export
minor_revision <- function(article, comments = "", date = Sys.Date()){
  article <- as.article(article)
  check_in_submission_folder(article)
  cli::cli_inform("Minor revision: {.field {article$id}}")
  update_status(article, "minor revision", comments = comments, date = date)

  email_template(article, "revision-minor")
  return(invisible(NULL))
}

check_in_submission_folder <- function(article){

  if (!inherits(article, "article")) cli::cli_abort("Not an article object.")

  if (!stringr::str_detect(article$path, "Submission")){
    cli::cli_abort("The article is not currently in the {.file Submissions} folder.
                   Do you input the article ID wrong?")
  }
}


#' Functions for proofing articles
#'
#' @details
#' \itemize{
#'    \item{\code{get_accepted_articles()}: get list of articles in the Accepted folder to be proofed. This can be used with \code{draft_proofing} to construct emails to authors on the final version.}
#'    \item{\code{draft_proofing()}: generate proofing email for one article}
#'    \item{\code{proofing_article()}: send proofing article emails}
#'    \item{\code{proofing_article_text()}: writes the email text into the correspondence folder}
#' }
#' @export
#' @rdname proofing
get_accepted_articles <- function() {
  # Warning: this gets all the articles that have been accepted
  # If any are not to appear in the issue need to work out how to ignore
  apath <- get_articles_path()
  acc <- list.files(file.path(apath, "Accepted"))

  cli::cat_line("Drafting proofing emails")
  draft_proofing(acc)

  invisible(NULL)
}

#' @param article this is the article id
#' @param update logical, if \code{TRUE} then the status is updated to "out for proofing"
#' @export
#' @rdname proofing
draft_proofing <- function(article, update=TRUE) {
  data <- as.data(as.article(article))
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 5, "%d %b %Y")

  template <- find_template("gmail_proofing")
  email <- whisker.render(readLines(template), data)
  if (update) update_status(data$id, "out for proofing")

  save_email(data, email)
  email_text(email)
}

# draft_proofing <- function(accepted) {
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
# }



#' @param drafts list of \code{gmail_draft} objects
#' @importFrom gmailr send_draft
#' @export
#' @rdname proofing
proofing_article <- function(drafts) {
  for (draft in drafts) {
    gmailr::send_draft(draft)
  }
  for (id in names(drafts)) {
    update_status(id, "out for proofing")
  }
  invisible(TRUE)
}


#' @param article article id
#' @export
#' @rdname proofing
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
