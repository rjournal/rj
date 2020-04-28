#' @importFrom stringr str_glue
msg_good <- function(x, ...){
  cat_line(str_glue(crayon::green("✔"), "  ", x, .envir = parent.frame()))
}

#' @importFrom stringr str_glue
msg_info <- function(x, ...){
  cat_line(str_glue(crayon::blue("ℹ"), "  ", x, .envir = parent.frame()))
}

#' @importFrom stringr str_glue
msg_bad <- function(x, ...){
  cat_line(str_glue(crayon::red("✖"), "  ", x, .envir = parent.frame()))
}

