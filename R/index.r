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
  index(articles)
}

index <- function(articles) {
  structure(list(articles = articles), class = "index")
}

"[.index" <- function(x, i) {
  index(x$articles[i])
}

"[[.index" <- function(x, i) {
  x$articles[[i]]
}

length.index <- function(x) length(x$articles)

format.index <- function(x, ...) {
  articles <- lapply(x$articles, format)
  paste(articles, collapse = "\n\n")
}
print.index <- function(x, ...) cat(format(x), "\n")
