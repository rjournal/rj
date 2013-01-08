#' Parse a string into a rfc822 address list.
#'
#' EBNF at \url{http://tools.ietf.org/html/rfc2822#section-3.4}
#'
#' @param string to parse
#' @return a list of \code{\link{address}}es
parse_address_list <- function(x) {
  stopifnot(is.character(x), length(x) == 1)
}

#' An S3 class to represent email addresses.
#'
#' @param email email address
#' @param name display name, optional
#' @export
#' @examples
#' address("h.wickham@@gmail.com")
#' address("h.wickham@@gmail.com", "Hadley Wickham")
#' parse_address("<h.wickham@@gmail.com> Hadley Wickham")
address <- function(email, name = NULL) {
  structure(list(name = name, email = email), class = "address")
}

print.address <- function(x, ...) {
  if (is.null(x$name)) {
    cat("<", x$email, ">\n", sep = "")
  } else {
    cat('"', x$name, '"', " <", x$email, ">\n", sep = "")
  }
}
