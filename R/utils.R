#' @importFrom stats setNames na.omit
#' @importFrom utils URLencode as.person browseURL
#' @importFrom utils capture.output type.convert zip
#' @importFrom utils edit glob2rx str unzip
#' @importFrom cli cli_alert_info cli_h1 cli_alert_danger

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

is.dir <- function(x) file.info(x)$isdir

in_dir <- function(path, code) {
  old <- setwd(path)
  on.exit(setwd(old))
  code
}

"%||%" <- function(a, b) if (empty(a)) b else a

"%NA%" <- function(a, b) ifelse(is.na(a), b, a)
