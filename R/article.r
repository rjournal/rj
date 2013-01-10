#' Create a new article (S3 class)
#'
#' This function will always succeed: if the article is not parseable it will
#' print an error message and return a unparsed blob. This ensures that
#' information is not lost even if some articles have parsing errors.
#'
#' @param ... Named arguments giving the components of an article:
#'   id, authors, title, editor, reviewers, status
#' @export
article <- function(..., quiet = FALSE) {
  tryCatch(make_article(...),
    error = function(e) {
      article <- unparsed(...)
      if (!quiet) {
        message("Failed to parse: ")
        print(article)
        message(e, "\n")
      }
      article
    }
  )
}

is.article <- function(x) inherits(x, "article")

make_article <- function(id, authors = "", title = "", editor = "",
                         reviewers = "", status = "") {
  structure(list(
    id = parse_id(id),
    authors = parse_address_list(authors),
    title = title,
    editor = editor,
    reviewers = parse_address_list(reviewers),
    status = parse_status_list(status)), class = "article")
}

format.article <- function(x, ...) {
  authors <- format(x$authors)
  reviewers <- format(x$reviewers)
  status <- format(x$status)

  paste(
    "ID: ", format(x$id), "\n",
    "Title: ", x$title, "\n",
    "Authors:", if (!empty(authors)) "\n  ", authors, "\n",
    "Editor: ", x$editor, "\n",
    "Reviewers:", if (!empty(reviewers)) "\n  ", reviewers, "\n",
    "Status: ", if (!empty(status)) "\n  ", status,
    sep = ""
  )
}

print.article <- function(x, ...) cat(format(x), "\n")

unparsed <- function(...) {
  structure(list(...), class = c("unparsed", "article"))
}

format.unparsed <- function(x, ...) {
  paste(
    "ID:", x$id, "\n",
    "Title:", x$title, "\n",
    "Authors:", x$authors, "\n",
    "Editor:", x$editor, "\n",
    "Reviewers:", x$reviewers, "\n",
    "Status:", x$status,
    sep = ""
  )
}

#' Generate a new article.
#'
#' @param index parsed index file
new_article <- function(index) {
  stopifnot(is.index(index))

  id <- new_id(index)
  make_article(id = id)
}
