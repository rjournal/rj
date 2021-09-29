list_reviewers <- function(article) {
  article <- as.article(article)
  reviewers <- article$reviewers
  cli::cli_ul()
  for (i in seq_along(reviewers)) {
    msg <- glue::glue("r{i}: {format(reviewers[[i]])}")
    cli::cli_li(msg)
  }
  cli::cli_end
  return(invisible(NULL))
}

#' Update reviewer's response to invite
#'
#' This function updates the reviewers field in the DESCRIPTION with reviewer's response:
#' accept, decline, or abandon if no reply from the reviewer for a period of time.
#' @inheritParams invite_reviewers
#' @examples
#' \dontrun{
#' # first reviewer declined
#' decline_reviewer("2020-114", reviewer_id = 1)
#'
#' # second reviewer agreed
#' agree_reviewer("2020-114", reviewer_id = 2)
#'
#' # third reviewer doesn't reply and deemed abandon
#' abandon_reviewer("2020-114", reviwer_id = 3)
#' }
#' @export
decline_reviewer <- function(article, reviewer_id) {
  check_dup_comment(article, reviewer_id, "Declined")
  comment <- paste("Declined", Sys.Date())
  add_reviewer_comment(article,
    reviewer_id = reviewer_id,
    comment = comment
  )
}

#' @rdname decline_reviewer
#' @export
agree_reviewer <- function(article, reviewer_id) {
  check_dup_comment(article, reviewer_id, "Agreed")
  comment <- paste("Agreed", Sys.Date())
  add_reviewer_comment(article,
    reviewer_id = reviewer_id,
    comment = comment
  )
}

#' @rdname decline_reviewer
#' @export
abandon_reviewer <- function(article, reviewer_id){
  check_dup_comment(article, reviewer_id, "Abandoned")
  comment <- paste("Abandoned", Sys.Date())
  add_reviewer_comment(article,
                       reviewer_id = reviewer_id,
                       comment = comment)
}

add_out_for_review <- function(article) {
  article <- as.article(article)
  status <- article$status
  latest_status <- article$status[[length(status)]]
  comments <- latest_status$comments
  if (!stringr::str_detect("out for review", comments)) {
    update_status(article, "out for review")
  }
  return(invisible(NULL))
}

#' Invite an reviewer
#'
#' This function adds the reviewer information(name and email) to the reviewers
#' field in the DESCRIPTION as well as draft an email to invite teh reviewer.
#' @param name Full name of the reviewer
#' @param invite Logical, whether to automatically construct an email to invite the reviewer
#' @inheritParams address
#' @inheritParams invite_reviewers
#' @examples
#' \dontrun{
#' add_reviewer("2020-114", "Przemyslaw Biecek", "przemyslaw.biecek@gmail.com")
#' }
#' @export
add_reviewer <- function(article, name, email, invite = TRUE) {
  article <- as.article(article)
  reviewers <- article$reviewers
  new_reviewer <- address(email = email, name = name)
  any_dups <- sapply(reviewers, identical, new_reviewer)
  if (any(any_dups)) {
    cli_alert_danger("Reviewer already added")
    reviewer_id <- which(any_dups)
  } else {
    reviewer_id <- length(reviewers) + 1
    cli_alert_info(paste0("Adding '", name, "' <", email, "> (of ", reviewer_id, ")"))
    reviewers[[reviewer_id]] <- new_reviewer
    article$reviewers <- address_list(reviewers)
    article <- save_article(article)
  }
  if (isTRUE(invite)) {
    invite_reviewer(article, reviewer_id = reviewer_id)
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
#' @param article Article id, like \code{"2014-01"}
#' @param reviewer_id Numeric, the index of the intended reviewer in the Reviewer field.
#' 1 for the first reviewer, 2 for the second
#' @param prefix Prefix added to start file name - used to distinguish
#'   between multiple rounds of reviewers (if needed)
#' @export
invite_reviewers <- function(article, prefix = "1") {
  article <- as.article(article)
  for (i in seq_along(article$reviewers)) {
    invite_reviewer(article, i, prefix = prefix)
  }
}

#' @rdname invite_reviewers
#' @export
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
    cli::cli_alert_info("Already invited - resending")
    email <- paste0(readLines(path), collapse = "\n")
  }

  # Update reviewer comment
  comment <- article$reviewers[[reviewer_id]]$comment
  test_string <- paste("Invited", Sys.Date())
  if (is.null(comment) || !stringr::str_detect(comment, pattern = test_string)) {
    add_reviewer_comment(article, reviewer_id = reviewer_id, test_string)
  }

  add_out_for_review(article)

  email_text(email)
}

#' Add the review file received from the reviewer
#'
#' This function adds the review file received from the reviewer to
#' the correspondence folder of the article.
#'
#' @param article Article id, like \code{"2014-01"}
#' @param reviewer_id Numeric, the index of the intended reviewer in the Reviewer field.
#' 1 for the first reviewer, 2 for the second
#' @param review Path to the review file, e.g. pdf, txt, or docx format. If not specified it is assumed that you added the new file into the correspondence directory and the last file for that reviewer will be used. If you specify \code{<i>-review-<j>.} filename (no path) and it already exists in the correspondence directory, it will be used.
#' @param recommend Reviewer's recommendation, one of: Accept, Minor, Major, Reject.
#' If not specified, an attempt is made to auto-detect it from the file by
#' looking at the first occurrence of those keywords. If auto-detect fails, use "Received".
#' @param date Date of the comment, defaults to today's date
#' @param AE Logical, if \code{TRUE} then \code{"AE: "} prefix is added to the recommendation.
#' @examples
#' \dontrun{
#' # add review file from the first reviewer and recommend accepting it
#' add_review("2020-114", reviewer_id = 1, review = file.choose(), recommend = "Accept")
#' }
#' @export
add_review <- function(article, reviewer_id, review, recommend = NULL, date = Sys.Date(), AE = is_AE()) {
  article <- as.article(article)
  dest <- file.path(article$path, "correspondence")
  copy <- TRUE
  ## if no review file is specified we assume the user wants to
  ## register the file they added in the corresondence directory
  if (missing(review)) {
    fn <- list.files(dest, paste0("^[0-9]+-review-", reviewer_id, "\\."))
    if (!length(fn)) stop("`review' is missing yet no *-review-", reviewer_id, "-.* file is present")
    review <- file.path(dest, fn[length(fn)])
    cli::cli_alert_info(paste0("Using existing file ", basename(review)))
    copy <- FALSE
  } else if (isTRUE(grepl(paste0("^[0-9]+-review-", reviewer_id, "\\."), review)) &&
    file.exists(file.path(dest, review))) {
    review <- file.path(dest, review)
    copy <- FALSE
    cli::cli_alert_info(paste0("Using existing file in correspondence ", basename(review)))
  }
  if (!file.exists(review)) {
    stop("Review file ", review, " doesn't exist")
  }

  if (copy) {
    # Determine the number of past reviews
    prefix <- length(list.files(dest, pattern = paste0("-review-", reviewer_id, "\\."))) + 1
    ext <- tools::file_ext(review)
    name <- paste0(prefix, "-review-", reviewer_id, ".", ext)
    path <- file.path(article$path, "correspondence", name)
    file.copy(review, to = path)
    cli::cli_alert_info(paste0("Created ", path))
  }
  if (is.null(recommend)) {
    guess <- tryCatch(suppressWarnings(readLines(review, 30, warn = FALSE, skipNul = TRUE)),
      error = function(e) ""
    )
    a.min <- grep("minor", guess, TRUE)
    a.maj <- grep("major", guess, TRUE)
    rej <- grep("reject", guess, TRUE)
    acc <- grep("accept", guess, TRUE)
    if (length(c(a.min, a.maj, rej, acc))) {
      recommend <- c("Minor", "Major", "Reject", "Accept")[
        suppressWarnings(which.min(c(min(a.min), min(a.maj), min(rej), min(acc))))
      ]
      cli::cli_alert_info(paste("Auto-detected recommendation:", recommend))
    } else {
      cli::cli_alert_info("Recommendation auto-detection, FAILED, using `Received'")
      recommend <- "Received"
    }
  }

  recommend <- tools::toTitleCase(recommend)
  if (recommend == "Accepted"){
    recommend <- "Accept"
    cli::cli_alert_info("Auto-correct recommendataion from `Accepted` to `Accept`")
  }
  if (!recommend %in% c("Accept", "Major", "Minor", "Reject")){
    cli::cli_abort("Recommandation is invalid, use one of `Accept`, `Major`, `Minor`, or `Reject`")
  }


  recommend <- paste(recommend, date)
  add_reviewer_comment(article,
    reviewer_id = reviewer_id,
    comment = recommend
  )
  return(invisible(NULL))
}

.review_info <- function(rev, id = "?") {
  cc <- strsplit(rev$comment, "; *")[[1]]
  st <- tolower(gsub("\\s+.*", "", cc, perl = TRUE))
  dt <- gregexpr("(\\d{4}-\\d{2}-\\d{2})", cc)
  m <- do.call(rbind, lapply(seq_along(dt), function(i) {
    o <- dt[[i]]
    if (any(o < 1)) {
      data.frame(date = NA, ext = NA, st = st[i])
    } else {
      if (length(o) > 2) {
        stop("Review entry ", i, " in ", id, " (", cc[i], ") has more than 2 dates.")
      }
      if (length(o) > 1) { ## assume one is the deadline the other the start
        d1 <- as.Date(substr(cc[i], o[1], o[1] + 10))
        d2 <- as.Date(substr(cc[i], o[2], o[2] + 10))
        ext <- abs(as.numeric(d1 - d2))
        data.frame(date = min(c(d1, d2)), ext = ext, st = st[i])
      } else {
        data.frame(date = as.Date(substr(cc[i], o[1], o[1] + 10)), ext = 0, st = st[i])
      }
    }
  }))
  #m$st[m$st == "accepted"] <- "agreed"
  m
}

#' @export
valid_reviewer_status <-
  c(
    "invited", "agreed", "declined", "abandoned", ## initial states
    "revision", ## like accepted for additional rounds after major
    "minor", "major", "reject", "accept"
  ) ## final state
# PS: "Accepted" is silently corrected to "Agreed" but it is
#     discouraged as it can be confused with the notion of Accept/Reject

#' Summarise reviewers' progression of an article
#'
#' This function summarises the status of reviewers who are willing to review
#' for a particular article.
#'
#' @param article Article id, like \code{"2014-01"}
#' @examples
#' reviewer_status("Submissions/2020-114")
#' @export
reviewer_status <- function(article) {
  d0 <- data.frame(date = as.Date(character()), ext = integer(), rid = integer(), fin = logical(), name = character(), email = character())
  article <- as.article(article)
  if (length(article$reviewers) < 0) {
    return(d0)
  }
  l <- lapply(article$reviewers, .review_info)
  for (i in seq_along(l)) {
    l[[i]]$rid <- i
  }
  lsl <- do.call(rbind, lapply(l, function(o) o[nrow(o), ]))
  if (any(!lsl$st %in% valid_reviewer_status)) {
    warning("Invalid review status in ", article$id[1], "-", article$id[2], ": ", paste(lsl$st[!lsl$st %in% valid_reviewer_status], collapse = ", "))
  }
  ## remove reviewers that declined
  lsl <- lsl[!lsl$st %in% c("declined", "abandoned"), ]
  if (length(lsl) && nrow(lsl)) {
    ## final state is minor/major/reject
    lsl$fin <- lsl$st %in% c("minor", "major", "reject", "accept")
    lsl$name <- sapply(lsl$rid, function(i) article$reviewers[[i]]$name)
    lsl$email <- sapply(lsl$rid, function(i) article$reviewers[[i]]$email)
    lsl
  } else {
    d0
  }
}
