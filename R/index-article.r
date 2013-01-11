find_article <- function(id, index = NULL) {
  stopifnot(is.character(id), length(id) == 1)
  index <- as.index(index)

  id <- parse_id(id)
  article <- Filter(function(x) identical(x$id, id), index$articles)
  if (length(article) == 0) {
    stop("Can't find ", format(id), call. = FALSE)
  }
  if (length(article) > 1) {
    stop("Found multiple articles with  ", format(id), call. = FALSE)
  }
  article[[1]]
}

update_article <- function(article, index = NULL) {
  stopifnot(is.article(article))
  index <- as.index(index)

  i <- Position(function(x) identical(x$id, article$id), index$articles)
  index$articles[[i]] <- article
  index
}
