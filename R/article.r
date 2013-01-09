#' Create a new article (S3 class)
#'
#' @param ... Named arguments giving the components of an article:
#'   id, authors, title, editor, reviewers, status
#' @param .recover If \code{TRUE}, will always succeed: if the article is not
#'   parseable it will return a unparsed blob. This ensures that information
#'   is not lost if some articles have parsing errors.
#' @export
article <- function(..., .recover = TRUE) {
  if (.recover) {
    tryCatch(make_article(...),
      error = function(e) {
        article <- unparsed(...)
        message("Failed to parse: ")
        print(article)
        message(e, "\n")
      })
  } else {
    make_article(...)
  }
}

make_article <- function(id, authors, title, editor, reviewers, status) {
  structure(list(
    id = parse_id(id),
    authors = parse_address_list(authors),
    title = title,
    editor = editor,
    reviewers = parse_address_list(reviewers),
    status = status), class = "article")
}

unparsed <- function(...) {
  structure(list(...), class = c("unparsed", "article"))
}

format.article <- function(x, ...) {
  paste(
    "ID: ", format(x$id), "\n",
    "Title: ", x$title, "\n",
    "Authors: \n  ", format(x$authors), "\n",
    "Editor: ", x$editor, "\n",
    "Reviewers: \n  ", format(x$reviewers), "\n",
    "Status: ", format(x$status), "\n",
    sep = ""
  )
}

print.article <- function(x, ...) cat(format(x), "\n")



