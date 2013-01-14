

empty <- function(x) UseMethod("empty")
empty.character <- function(x) str_length(x) == 0
empty.address_list <- function(x) length(x) == 0

#' @importFrom stringr str_c str_length str_sub
str_trunc <- function(x, width = getOption("width")) {
  ifelse(str_length(x) <= width, x, str_c(str_sub(x, 1, width - 3), "..."))
}
