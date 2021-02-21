#' Set the directory of the articles repository
#'
#' This path is used to locate articles on your file system. If this path is not
#' specified, the path will default to the current working directory if it is
#' named "articles".
#' @param path Articles path
#' @export
set_articles_path <- function(path) {
  .articles$path <- normalizePath(path)
}

#' Get the directory of the articles repository
#'
#' This path is used to point to articles on your file system.
#' @export
get_articles_path <- function() {
  dir <- .articles$path %||% getwd()
  if (!(grepl("articles", basename(dir)))) {
    abort(
      sprintf("The current articles path is not correct.
              Set the path to the articles repository with `set_articles_path()`",
              dir)
    )
  }
  dir
}

# Articles settings
.articles <- new.env()
