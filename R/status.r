final_status <- c(
  "published",
  "withdrawn",
  "rejected"
)

valid_status <- c(
  "submitted",
  "acknowledged",
  "updated",
  "out for review",
  "major revision",
  "minor revision",
  "accepted",
  "copy edited",
  "proofed",
  "online",
  final_status
)

parse_status_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  if (empty(str_trim(x))) return(status_list())

  statuses <- str_trim(str_split(x, ",")[[1]])
  statuses <- statuses[statuses != ""]

  status_list(lapply(statuses, parse_status))
}

status_list <- function(x = list()) {
  structure(list(status = x), class = "status_list")
}

length.status_list <- function(x) length(x$status)
"[[.status_list" <- function(x, i) x$status[[i]]

format.status_list <- function(x, ...) {
  statuses <- lapply(x$status, format)
  paste(statuses, collapse = ",\n  ")
}
print.status_list <- function(x, ...) cat(format(x), "\n")
is.status_list <- function(x) inherits(x, "status_list")

parse_status <- function(x) {
  x <- str_trim(x)

  re <- "^(\\d{4}-\\d{2}-\\d{2}) ([^\\[]*)(?: \\[([^\\[]+)\\])?$"
  if (!str_detect(x, re)) {
    stop("Status must have form 'yyyy-mm-dd status [optional comments]'",
      call. = FALSE)
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
    guess <- amatch_status(status)
    if (tolower(status) == tolower(guess)) {
      status <- guess
    } else {
      warning(status, " is not a known status. ",
        "Did you mean ", amatch_status(status), "?", call. = FALSE)
    }

  }

  status(status = status, date = date, comments = comments)
}

status <- function(status, date = Sys.Date(), comments = "") {
  stopifnot(is.Date(date), length(date) == 1)
  stopifnot(is.character(status), length(status) == 1)
  stopifnot(is.character(comments), length(comments) == 1)

  structure(list(date = date, status = status, comments = comments),
    class = "status")
}

is.status <- function(x) inherits(x, "status")

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

format.status <- function(x, ...) {
  paste(format(x$date), " ", x$status,
    if (!empty(x$comments)) paste(" [", x$comments, "]", sep = ""), sep = "")
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
