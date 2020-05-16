list_reviewers = function(article) {
  article = as.article(article)
  reviewers = article$reviewers
  cli::cli_ul()
  for (i in seq_along(reviewers)) {
    msg = glue::glue("r{i}: {format(reviewers[[i]])}")
    cli::cli_li(msg)
  }
  cli::cli_end
  return(invisible(NULL))
}

#' @title Review helper function
#' @inheritParams invite_reviewers
#' @export
decline_reviewer = function(article, reviewer_id) {
  check_dup_comment(article, reviewer_id, "Declined")
  comment = paste("Declined", Sys.Date())
  add_reviewer_comment(article,
                       reviewer_id = reviewer_id,
                       comment = comment)
}

#' @rdname decline_reviewer
#' @export
agree_reviewer = function(article, reviewer_id) {
  check_dup_comment(article, reviewer_id, "Agreed")
  comment = paste("Agreed", Sys.Date())
  add_reviewer_comment(article,
                       reviewer_id = reviewer_id,
                       comment = comment)
}


#' Add review to DESCRIPTION
#'
#' Add a reviewers name to the DESCRIPTION file
#' @param invite Automatically construct email
#' @inheritParams address
#' @inheritParams invite_reviewers
#' @export
add_reviewer = function(article, name, email, invite = TRUE) {
  article = as.article(article)
  reviewers = article$reviewers
  new_reviewer = address(email = email, name = name)
  any_dups = sapply(reviewers, identical, new_reviewer)
  if (any(any_dups)) {
    cli_alert_danger("Reviewer already added")
  } else {
    total_reviewers = length(reviewers) + 1
    cli_alert_info(paste0("Adding '", name, "' <", email, "> (of ", total_reviewers, ")"))
    reviewers[[total_reviewers]] =  new_reviewer
    article$reviewers = address_list(reviewers)
    article = save_article(article)
  }
  if (isTRUE(invite)) {
    invite_reviewer(article, reviewer_id = total_reviewers)
  }
  return(invisible(article))
}

#' Invite reviewer(s).
#'
#' Once you have added reviewers to the DESCRIPTION file, you can use
#' this function to draft invite emails to them all. As well as drafting
#' the emails, it also caches them locally in the \code{correspodence/}
#' directory of the corresponding article.
#'
#' @param article article id, like \code{"2014-01"}
#' @param reviewer_id invite just a single reviewer
#' @param prefix prefix added to start file name - used to distinguish
#'   between multiple rounds of reviewers (if needed)
#' @export
invite_reviewers <- function(article, prefix = "1") {
  article <- as.article(article)
  update_status(article, "out for review")

  for (i in seq_along(article$reviewers)) {
    invite_reviewer(article, i, prefix = prefix)
  }
}

#' @export
#' @rdname invite_reviewers
invite_reviewer <- function(article, reviewer_id, prefix = "1") {
  article <- as.article(article)

  dest <- file.path(article$path, "correspondence")
  if (!file.exists(dest)) dir.create(dest)

  reviewer <- article$reviewers[[reviewer_id]]
  name <- paste0(prefix, "-invite-", reviewer_id, ".txt")
  path <- file.path(dest, name)

  if (!file.exists(path)) {
    data <- as.data(article)
    data$email <- reviewer$email
    data$name <- reviewer$name
    data$firstname <- stringr::str_split(reviewer$name, " ")[[1]][1]
    data$date <- format(Sys.Date() + 30, "%d %b %Y")

    template <- find_template("review")
    email <- whisker.render(readLines(template), data)

    writeLines(email, path)
  } else {
    message("Already invited - resending")
    email <- paste0(readLines(path), collapse = "\n")
  }

  # Update reviewer comment
  comment = article$reviewers[[reviewer_id]]$comment
  test_string = paste("Invited", Sys.Date())
  if (!stringr::str_detect(comment, pattern = test_string)) {
    add_reviewer_comment(article, reviewer_id = reviewer_id, test_string)
  }
  email_text(email)
}

#' @rdname invite_reviewers
#' @param review Path to the review, e.g. pdf, txt, or docx format
#' @param recommend Summary of review. Accept, minor, major, reject
#' @export
add_review = function(article, reviewer_id, review, recommend = NULL) {
  article = as.article(article)
  dest = file.path(article$path, "correspondence")
  # Determine the number of past reviewers
  prefix = length(list.files(dest, pattern = paste0("-.*-", reviewer_id, "\\."))) + 1
  # TODO: Check for duplicates
  if (!file.exists(review)) {
    stop(review, " doesn't exist")
  }
  ext = tools::file_ext(review)
  name = paste0(prefix, "-review-", reviewer_id, ".", ext)
  path = file.path(article$path, name)
  file.copy(review, to = path)
  cli::cli_alert_info(paste0("Created ", path))
  if (is.null(recommend)) {
    recommend = "Received"
  }
  recommend = paste(recommend, Sys.Date())
  add_reviewer_comment(article, reviewer_id = reviewer_id,
                       comment = recommend)
  return(invisible(NULL))
}
