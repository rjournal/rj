empty <- function(x) UseMethod("empty")
#' @S3method empty character
empty.character <- function(x) str_length(x) == 0
#' @S3method empty address_list
empty.address_list <- function(x) length(x) == 0
#' @S3method empty NULL
empty.NULL <- function(x) TRUE

str_trunc <- function(x, width = getOption("width")) {
  ifelse(str_length(x) <= width, x, str_c(str_sub(x, 1, width - 3), "..."))
}

is.dir <- function(x) file.info(x)$isdir


in_dir <- function(path, code) {
  old <- setwd(path)
  on.exit(setwd(old))
  code
}

"%||%" <- function(a, b) if (empty(a)) b else a
