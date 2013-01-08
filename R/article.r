#' Create a new article (S3 class)
#'
#' @export
article <- function(id, authors, title, editor, reviewers, status) {
  structure(list(
    id = id,
    authors = parse_address_list(authors),
    title = title,
    editor = editor,
    reviewers = parse_address_list(reviewers),
    status = status), class = "article")
}
