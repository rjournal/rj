#' Set the directory of the articles repository
#'
#' This path is used to locate articles on your file system. If this path is not
#' specified, the path will default to the current working directory if it is
#' named "articles".
#'
#' @export
set_articles_path <- function(path){
  .articles$path <- path
}

get_articles_path <- function(){
  dir <- .articles$path %||% getwd()
  if(!(basename(dir) %in% c("articles", "articles_test"))){
    abort(
      sprintf("The current articles path is not correct. Set the path to the articles repository with `set_articles_path()`",
              dir)
    )
  }
  dir
}

# Articles settings
.articles <- new.env()
