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
  "needs reviewers",
  "needs editor",
  "updated",
  "out for review",
  "major revision",
  "minor revision",
  "accepted",
  "copy edited",
  "online",
  "proofed",
  "style checked",
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

  if (date > Sys.Date()) stop("Date must not be in the future")
  if (date < as.Date("2002-01-01")) {
    stop("Date must not before the R journal was created")
  }

  status <- str_trim(status)
  if (!(status %in% valid_status)) {
    guess <- amatch_status(status)
    if (tolower(status) == tolower(guess)) {
      status <- guess
    } else {
      stop(status, " is not a known status. ",
        "Did you mean ", amatch_status(status), "?", call. = FALSE)
    }
  }

  structure(list(date = date, status = status, comments = comments),
    class = "status")
}

is.status <- function(x) inherits(x, "status")

#' @export
c.status <- c.status_list <- function(..., recursive = FALSE) {
  pieces <- list(...)
  statuses <- lapply(pieces, function(x) {
    if (is.status(x)) {
      list(x)
    } else if (is.status_list(x)) {
      x$status
    } else {
      stop("Don't know how to combine with ", class(x)[1])
    }
  })
  status_list(unlist(statuses, recursive = FALSE))
}

#' @export
format.status <- function(x, ...) {
  paste(format(x$date), " ", x$status,
    if (!empty(x$comments)) paste(" [", x$comments, "]", sep = ""), sep = "")
}
#' @export
print.status <- function(x, ...) cat(format(x), "\n")

#' @importFrom utils adist
amatch_status <- function(status) {
  ldist <- adist(status, valid_status, ignore.case = TRUE, partial = FALSE,
    costs = c(ins = 0.5, sub = 1, del = 2))[1, ]
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
  if (empty(x$editor)) {
    "needs editor (editor-in-chief)"
  } else if (empty(x$reviewers)) {
    "needs reviewers (editor)"
  } else {
    switch(status,
      "major revision" = "waiting (author)",
      "minor revision" = "waiting (author)",
      "out for review" = "waiting (reviewers)",
      "updated" = "waiting (editor)",

      "reject and resubmit" = "waiting (author)",
      "published" = "needs removal (editor)",
      "withdrawn" = "needs removal (editor)",
      "rejected" = "needs removal (editor)",

      "accepted" = "waiting (author)",
      "style checked" = "needs online (editor-in-chief)",
      "online" = "needs copy-editing (editor)",
      "copy edited" = "waiting (author)",
      "proofed" = "ready for publication (editor-in-chief)",
      "acknowledged" = "needs reviewers (editor)",
      "submitted" = "needs acknowledgement (editor-in-chief)",

      stop("Unknown status: ", status)
    )
  }
}

# Takes a summary status as input, and returns number of days before it's due
deadlines <- function(sstatus) {
  if (sstatus %in% final_status) {
    return(c(Inf, Inf))
  }

  # > 1st = *; > 2nd = **
  special <- list(
    "needs editor" = c(7L, 14L),
    "needs reviewers" = c(7L, 14L),
    "submitted" = c(3L, 7L),
    "proofed" = c(7L, 14L),
    "major revision" = c(60L, 90L)
  )
  if (sstatus %in% names(special)) {
    special[[sstatus]]
  } else {
    c(4L, 6L) * 7L
  }
}

# status_list class ------------------------------------------------------------

status_list <- function(x = list()) {
  structure(list(status = x), class = "status_list")
}

#' @export
length.status_list <- function(x) length(x$status)
#' @export
"[[.status_list" <- function(x, i) x$status[[i]]

#' @export
format.status_list <- function(x, ...) {
  statuses <- lapply(x$status, format)
  paste(statuses, collapse = ",\n  ")
}
#' @export
print.status_list <- function(x, ...) cat(format(x), "\n")
is.status_list <- function(x) inherits(x, "status_list")

# Parsing ----------------------------------------------------------------------

parse_status_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  x <- trimws(x)
  if (empty(x)) return(status_list())

  statuses <- trimws(strsplit(x, ",[ \t\r]*(\n|$)")[[1]])
  statuses <- statuses[statuses != ""]

  status_list(lapply(statuses, parse_status))
}

parse_status <- function(x) {
  x <- str_trim(x)

  re <- "^(\\d{4}-\\d{2}-\\d{2}) ([^\\[]*)(?: \\[([^\\[]+)\\])?$"
  if (!str_detect(x, re)) {
    stop("Status must have form 'yyyy-mm-dd status [optional comments]'",
      call. = FALSE)
  }

  pieces <- str_match(x, re)[1, ]

  date <- pieces[2]
  if (!is.date(date)) stop("Date must be a valid date")
  date <- as.Date(date)

  status <- pieces[3]
  comments <- if (is.na(pieces[4])) "" else pieces[4]

  status(status = status, date = date, comments = comments)
}

as.data.frame.status_list <- function(status_list) {
  ml <- vector(mode="list", length=length(status_list))
  for (i in seq(along=ml)) ml[[i]] <- as.data.frame(unclass(status_list[[i]]))
  do.call("rbind", ml)
}
