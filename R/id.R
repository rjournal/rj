parse_id <- function(x) {
  if (is.id(x)) {
    return(x)
  }

  re <- "^([0-9]{4})-([0-9]{2,3})[a-z]?$"

  x <- str_trim(x)

  if (!str_detect(x, re)) stop("ID must have form XXXX-YYY?")

  pieces <- str_match(x, re)[1, ]
  year <- pieces[2]
  seq <- pieces[3]

  if (!is.number(year)) stop("Year must be a number")
  if (!is.number(seq)) stop("ID must be a number")

  if (year > year()) stop("Year must be in the present or past")
  if (year < 2002) stop("Year must be >= 2002")

  id(as.integer(year), as.integer(seq))
}

id <- function(year, seq) {
  stopifnot(is.numeric(year), length(year) == 1)
  stopifnot(is.numeric(seq), length(seq) == 1)

  year <- as.integer(year)
  seq <- as.integer(seq)

  structure(list(year = year, seq = seq), class = "id")
}

is.id <- function(x) inherits(x, "id")

#' @method format id
#' @export
format.id <- function(x, ...) {
  paste(x$year, sprintf("%02d", x$seq), sep = "-")
}
#' @method print id
#' @export
print.id <- function(x, ...) cat(format(x), "\n")

year <- function() as.POSIXlt(Sys.Date())$year + 1900

is.number <- function(x) {
  suppressWarnings(!is.na(as.numeric(x)))
}

#' @method as.character id
#' @export
as.character.id <- function(x, ...)
  format.id(x)
