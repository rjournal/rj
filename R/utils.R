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

issue_month <- function(issue) {
  issue_regex <- "^(\\d{4})-(\\d{1})"
  issue_year <- as.integer(sub(issue_regex, "\\1", issue))
  issue_num <- as.integer(sub(issue_regex, "\\2", issue))

  issue_months <- if(issue_year < 2022) {
    c("June", "December")
  } else {
    c("March", "June", "September", "December")
  }
  issue_months[issue_num]
}

partition_rmd <- function(file) {
  input <- xfun::read_utf8(file)
  front_matter_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input)

  list(
    front_matter = yaml::yaml.load(input[(front_matter_delimiters[1L]+1L):(front_matter_delimiters[2L]-1)]),
    body = input[(front_matter_delimiters[2L]+1L):length(input)]
  )
}


update_front_matter <- function(yml, file) {
  input <- xfun::read_utf8(file)
  front_matter_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input)

  xfun::write_utf8(
    c(
      "---",
      yaml::as.yaml(yml),
      "---",
      "",
      input[(front_matter_delimiters[2]+1):length(input)]
    ),
    file
  )
}
