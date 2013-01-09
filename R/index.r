#' Parse the index.dcf file.
#'
#' @examples
#' parse_index(sample())
parse_index <- function(path = "index.dcf") {
  dcf <- read.dcf(path,
    fields = c("ID", "Authors", "Title", "Editor", "Reviewers", "Status"))
  colnames(dcf) <- tolower(colnames(dcf))

  n <- nrow(dcf)
  articles <- lapply(seq_len(n), function(i) do.call(article, as.list(dcf[i, ])))
  index(articles, path)
}

save_index <- function(index) {
  stopifnot(is.index(index))

  cat(format(index), "\n", file = index$path)
}

index <- function(articles, path) {
  structure(list(articles = articles, path = path), class = "index")
}

"[.index" <- function(x, i) {
  index(x$articles[i])
}

"[[.index" <- function(x, i) {
  x$articles[[i]]
}

c.index <- function(..., recursive = FALSE) {
  pieces <- list(...)

  index <- pieces[[1]]
  for(i in seq.int(2L, length(pieces))) {
    piece <- pieces[[i]]
    if (is.index(piece)) {
      index$articles <- c(index$articles, piece$articles)
    } else if (is.article(piece)) {
      index$articles <- c(index$articles, list(piece))
    } else {
      stop("Don't know how to combine")
    }
  }

  index
}

is.index <- function(x) inherits(x, "index")

length.index <- function(x) length(x$articles)

format.index <- function(x, ...) {
  articles <- lapply(x$articles, format)
  paste(articles, collapse = "\n\n")
}
print.index <- function(x, ...) cat(format(x), "\n")
