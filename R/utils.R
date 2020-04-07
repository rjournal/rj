#' @importFrom stats setNames na.omit
#' @importFrom utils URLencode as.person browseURL
#' @importFrom utils capture.output type.convert zip
#' @importFrom utils edit glob2rx str unzip

empty <- function(x) UseMethod("empty")
#' @method empty character
#' @export 
empty.character <- function(x) str_length(x) == 0
#' @method empty address_list
#' @export
empty.address_list <- function(x) length(x) == 0
#' @method empty NULL
#' @export
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
