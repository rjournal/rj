#' Set the directory of the articles repository
#'
#' This path is used to locate articles on your file system. If this path is not
#' specified, the path will default to the root of the repository which contains
#' the working directory
#' @param path Articles path
#' @export
set_articles_path <- function(path) {
  .articles$path <- normalizePath(path.expand(path))
}

#' Get the directory of the articles repository, either as set by
#' set_articles_path() or using git to determine repository root
#'
#' This path is used to point to articles on your file system.
#' @export
get_articles_path <- function() {
  dir <- .articles$path
  if (!is.null(.articles$path)) {
    if (!dir.exists(dir)) {
      warning("Articles path set via set_articles_path() does not exits, ignoring.")
      dir <- NULL
    }
  }

  ## not set or not valid, use git
  if (is.null(dir)) {
    dir <- try(system("git rev-parse --show-toplevel", intern = TRUE),
      silent = TRUE
    )
    if (inherits(dir, "try-error")) {
      abort("Current directory is not a git repository, use set_articles_path() if you don't have wokring git (error: ", dir)
    }
  }
  dir
}

# Articles settings
.articles <- new.env()
