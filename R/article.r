#' Create a new article (S3 class)
#'
#' @export
article <- function(id, authors, title, editor, reviewers, status) {
  structure(list(id = id, authors = authors, title = title, editor = editor,
    reviewers = reviewers, status = status), class = "article")
}
