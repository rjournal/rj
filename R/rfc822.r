#' Parse a string into a rfc822 address list.
#'
#' EBNF at \url{http://tools.ietf.org/html/rfc2822#section-3.4}
#'
#' @param string to parse
#' @return a list of \code{\link{address}}es
#' @examples
#' parse_address_list("<a@@b.com> Alison, <c@@d.com> Colin")
parse_address_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
  if (empty(x)) return(address_list())

  address_list(lapply(str_split(x, ",")[[1]], parse_address))
}

address_list <- function(addresses = list()) {
  stopifnot(is.list(addresses))
  structure(addresses, class = "address_list")
}

format.address_list <- function(x, ...) {
  addresses <- vapply(x, format, character(1))
  paste(addresses, collapse = ",\n  ")
}
print.address_list <- function(x, ...) cat(format(x), "\n")

#' An S3 class to represent email addresses.
#'
#' @param email email address
#' @param name display name, optional
#' @export
#' @examples
#' address("h.wickham@@gmail.com")
#' address("h.wickham@@gmail.com", "Hadley Wickham")
#' parse_address("Hadley Wickham <h.wickham@@gmail.com>")
#' parse_address("<h.wickham@@gmail.com>")
address <- function(email = NULL, name = NULL) {
  if (is.null(email) && is.null(name)) {
    stop("Address must have name or email", call. = FALSE)
  }

  structure(list(name = name, email = email), class = "address")
}

format.address <- function(x, ...) {
  name <- if (!is.null(x$name))    paste('"', x$name, '"', sep = "")
  email <- if (!is.null(x$email))  paste("<", x$email, ">", sep = "")

  paste(c(name, email), collapse = " ")
}

print.address <- function(x, ...) cat(format(x), "\n")

#' @rdname address
parse_address <- function(x) {
  stopifnot(is.character(x), length(x) == 1)

  pieces <- str_match(x, "^\\s*([^<>]*) ?(<.*>)?")[1, ]

  email <- str_trim(pieces[3])
  email <- str_replace_all(email, "<|>", "")
  if (is.na(email) || str_length(email) == 0) email <- NULL

  name <- str_trim(pieces[2])
  name <- str_replace_all(name, fixed('"'), "")
  if (is.na(name) || str_length(name) == 0) name <- NULL

  address(email, name)
}

