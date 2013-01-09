final_status <- c(
  "Published",
  "Withdrawn",
  "Rejected"
)

valid_status <- c(
  "Submitted",
  "Acknowledged",
  "Needs editor",
  "Needs reviewers",
  "Under review",
  "Major revision",
  "Minor revision",
  "Accepted",
  "Copy edited",
  "Proofed",
  final_status
)

parse_statuses <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  statuses <- str_split(x, ",")[[1]]

  lapply(statuses, parse_status)
}

parse_status <- function(x) {
  x <- str_trim(x)

  re <- "^(\\d{4}-\\d{2}-\\d{2}) ([^\\[]*)(?: \\[([^\\[]+)\\])?$"
  if (!str_detect(x, re)) {
    stop("ID must have form 'yyyy-mm-dd status [optional comments]'")
  }

  pieces <- str_match(x, re)[1, ]
  date <- pieces[2]
  status <- pieces[3]
  comments <- pieces[4]

  if (!is.date(date)) stop("Date must be a valid date")
  date <- as.Date(date)
  if (date > Sys.Date()) stop("Date must not be in the future")
  if (date < as.Date("2002-01-01")) {
    stop("Date must not before the R journal was created")
  }

  status <- str_trim(status)
  if (!(status %in% valid_status)) {
    warning(status, " is not a known status. ",
      "Did you mean ", amatch_status(status), "?", call. = FALSE)
  }

  status(date, status, comments)
}

status <- function(date, status, comments = "") {
  stopifnot(is.Date(date), length(date) == 1)
  stopifnot(is.character(status), length(status) == 1)
  stopifnot(is.character(comments), length(comments) == 1)

  structure(list(date = date, status = status, comments = comments),
    class = "status")
}

format.status <- function(x, ...) {
  paste(format(x$date), x$status,
    if (!empty(x$comments)) paste("[", x$comments, "]", sep = ""))
}
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
