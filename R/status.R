final_status <- c(
  "reject and resubmit",
  "published",
  "withdrawn",
  "rejected"
)

#' A list of all valid statuses.
#'
#' @export
#' @examples
#' valid_status
valid_status <- c(
  "submitted",
  "acknowledged",
  "passed initial checks",
  "resubmission",
  "needs reviewers",
  "needs editor",
  "updated",
  "out for review",
  "major revision",
  "minor revision",
  "revision received",
  "accepted",
  "copy edited",
  "online",
  "proofed",
  "out for proofing",
  "style checked",
  "with AE",
  "AE: major revision",
  "AE: minor revision",
  "AE: accept",
  "AE: reject",
  final_status
)

# Status class and methods -----------------------------------------------------

#' Create a S3 status object
#'
#' @param status A string description the status. Must be listed in
#'   \code{\link{valid_status}}
#' @param date Date, defaults to today. Must be after 2002-01-01 and
#'   not in the future.
#' @param comments any additional extra comments
#' @keywords internal
#' @export
#' @examples
#' status("rejected")
#' c(status("rejected"), status("accepted"))
status <- function(status, date = Sys.Date(), comments = "") {
  stopifnot(is.Date(date), length(date) == 1)
  stopifnot(is.character(status), length(status) == 1)
  stopifnot(is.character(comments), length(comments) == 1)

  # Date + 1 provides a buffer for timezones with remote resources.
  if (date > (Sys.Date() + 1)) stop("Date must not be in the future")
  if (date < as.Date("2002-01-01")) {
    stop("Date must not before the R journal was created")
  }

  status <- str_trim(status)
  if (!(status %in% valid_status)) {
    guess <- amatch_status(status)
    if (tolower(status) == tolower(guess)) {
      status <- guess
    } else {
      stop(
        status,
        " is not a known status. ",
        "Did you mean ",
        amatch_status(status),
        "?",
        call. = FALSE
      )
    }
  }

  structure(
    list(date = date, status = status, comments = comments),
    class = "status"
  )
}

is.status <- function(x) inherits(x, "status")


#' @export
c.status <- c.status_list <- function(..., recursive = FALSE) {
  pieces <- list(...)
  statuses <- lapply(pieces, function(x) {
    if (is.status(x)) {
      list(x)
    } else if (is.status_list(x)) {
      x
    } else {
      stop("Don't know how to combine with ", class(x)[1])
    }
  })

  status_list(unlist(statuses, recursive = FALSE))
}

#' @export
format.status <- function(x, ...) {
  paste(
    format(x$date),
    " ",
    x$status,
    if (!empty(x$comments)) paste(" [", x$comments, "]", sep = ""),
    sep = ""
  )
}
#' @export
print.status <- function(x, ...) cat(format(x), "\n")

#' @importFrom utils adist
amatch_status <- function(status) {
  ldist <- adist(
    status,
    valid_status,
    ignore.case = TRUE,
    partial = FALSE,
    costs = c(ins = 0.5, sub = 1, del = 2)
  )[1, ]
  valid_status[which.min(ldist)]
}

is.date <- function(x) {
  parsed <- strptime(x, "%Y-%m-%d")
  !is.na(parsed) && format(parsed) == x
}

is.Date <- function(x) inherits(x, "Date")


# Reporting --------------------------------------------------------------------

todo <- function(x) {
  stopifnot(is.article(x))

  status <- last_status(x)$status
  status_date <- last_status(x)$date
  if (
    status %in%
      c(
        "resubmission",
        "reject and resubmit",
        "major revision",
        "minor revision",
        "accepted",
        "copy edited"
      )
  ) {
    "waiting (author)"
  } else if (
    empty(x$editor) |
      status == "submitted" |
      (status == "revision received" & status_date > "2025-01-01")
  ) {
    # Needs an editor, or needs an acknowledgement.
    # Ignore unacknowledged revisions before 2025
    "waiting (editor-in-chief)"
  } else if (status == "with AE") {
    "waiting (AE)"
  } else if (empty(x$reviewers)) {
    "waiting (editor)"
  } else if (status == "out for review" & completed_reviews(x) >= 2) {
    "waiting (editor)"
  } else {
    switch(
      status,
      "out for review" = "waiting (reviewers)",
      "updated" = "waiting (editor)",
      "published" = "needs removal (editor)",
      "withdrawn" = "needs removal (editor)",
      "rejected" = "needs removal (editor)",
      "revision received" = "waiting (editor)",
      "AE: major revision" = "waiting (editor)",
      "AE: minor revision" = "waiting (editor)",
      "AE: accept" = "waiting (editor)",
      "AE: reject" = "waiting (editor)",
      "style checked" = "needs online (editor-in-chief)",
      "online" = "needs copy-editing (editor)",
      "proofed" = "ready for publication (editor-in-chief)",
      "acknowledged" = "needs reviewers (editor)",
      stop("Unknown status: ", status)
    )
  }
}

# Takes a summary status as input, and returns number of days before it's due
deadlines <- function(sstatus) {
  if (sstatus %in% final_status) {
    return(rep(Inf, 3))
  }

  # > 1st = *; > 2nd = **; > 3rd = ***
  special <- list(
    "needs editor" = c(7L, 14L, 28L),
    "needs reviewers" = c(7L, 14L, 28L),
    "submitted" = c(3L, 7L, 28L),
    "proofed" = c(7L, 14L, 28L),
    "major revision" = c(60L, 90L, 180L),
    "waiting (AE)" = c(60L, 90L, 150L),
    "waiting (editor)" = c(7L, 14L, 28L),
    "waiting (editor-in-chief)" = c(7L, 14L, 21L)
  )
  if (sstatus %in% names(special)) {
    special[[sstatus]]
  } else {
    c(4L, 6L, 26) * 7L
  }
}

# status_list class ------------------------------------------------------------

status_list <- function(x = list()) {
  structure(x, class = "status_list")
}

#' @export
format.status_list <- function(x, ...) {
  statuses <- lapply(x, format)
  paste(statuses, collapse = ",\n  ")
}

#' @export
print.status_list <- function(x, ...) {
  statuses <- lapply(x, format)
  cat(paste(statuses, collapse = "\n"))
}
is.status_list <- function(x) inherits(x, "status_list")

# Parsing ----------------------------------------------------------------------

parse_status_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  x <- trimws(x)
  if (empty(x)) {
    return(status_list())
  }

  statuses <- trimws(strsplit(x, ",[ \t\r]*(\n|$)")[[1]])
  statuses <- statuses[statuses != ""]

  status_list(lapply(statuses, parse_status))
}

parse_status <- function(x) {
  x <- stringr::str_trim(x)

  re <- "^(\\d{4}-\\d{2}-\\d{2}) ([^\\[]*)(?: \\[([^\\[]+)\\])?$"
  if (!stringr::str_detect(x, re)) {
    # NM added line
    cat("bad status:", x, "\n")
    stop(
      "Status must have form 'yyyy-mm-dd status [optional comments]'",
      call. = FALSE
    )
  }

  pieces <- stringr::str_match(x, re)[1, ]

  date <- pieces[2]
  if (!is.date(date)) stop("Date must be a valid date")
  date <- as.Date(date)

  status <- pieces[3]
  comments <- if (is.na(pieces[4])) "" else pieces[4]

  status(status = status, date = date, comments = comments)
}

#' @export
as.data.frame.status_list <- function(x, ...) {
  message("DF")
  ml <- vector(mode = "list", length = length(x))
  for (i in seq(along = ml)) ml[[i]] <- as.data.frame(unclass(x[[i]]))
  do.call("rbind", ml)
}
