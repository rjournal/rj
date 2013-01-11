#' Parse the index.dcf file.
#'
#' @examples
#' index <- parse_index(sample())
#' index
#' save_index(index)
parse_index <- function(path = "index.dcf", quiet = FALSE) {
  message("Reading ", path)

  fields <- c("ID", "Authors", "Title", "Editor", "Reviewers", "Status")
  dcf <- read.dcf(path, fields = fields, keep.white = fields)

  # Remove field names that keep.white incorrectly preserves
  for(field in fields) {
    dcf[, field] <- gsub(paste(field, ":", sep = ""), "", dcf[, field],
      fixed = TRUE)
  }
  # Convert missing values to empty strings
  dcf[is.na(dcf)] <- ""
  colnames(dcf) <- tolower(colnames(dcf))

  n <- nrow(dcf)
  articles <- lapply(seq_len(n), function(i) {
    args <- c(as.list(dcf[i, ]), list(quiet = quiet))
    do.call(article, args)
  })
  index(articles, path)
}

as.index <- function(index) {
  if (is.index(index)) return(index)

  path <- find_index(index)
  parse_index(path)
}

#' Find the index file.
#'
#' Looks first in \code{path} and then in the \code{index.dcf}
#' option.
#'
#' @export
find_index <- function(path = NULL) {
  if (!is.null(path)) {
    if (!file.exists(path)) stop(path, " not found", call. = FALSE)
    return(path)
  }

  opt <- getOption("index.dcf")
  if (!is.null(opt)) return(opt)

  stop("Can't find index.dcf", call. = FALSE)
}

save_index <- function(index) {
  stopifnot(is.index(index))
  stopifnot(!is.null(index$path))

  path <- index$path
  message("Writing ", path)
  cat(format(index), "\n", file = path)
}

index <- function(articles, path = NULL) {
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
