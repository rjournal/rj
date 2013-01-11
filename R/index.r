#' Load/save the index.dcf file.
#'
#' An index object saves both the contents and the location of the file, so
#' it can easily be resaved.
#'
#' @param path to file to load
#' @param quiet if \code{TRUE} no output is produced.
#' @examples
#' index <- load_index(sample())
#' index
#' save_index(index)
load_index <- function(path, quiet = FALSE) {
  if (!quiet) message("Reading ", path)

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

#' Find the index file.
#'
#' Looks first in \code{path} and then in the \code{index.dcf}
#' option.
#'
#' @return For \code{find_index} a path, for \code{as.index} the
#'   parsed index file.
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

#' @rdname find_index
#' @export
as.index <- function(index) {
  if (is.index(index)) return(index)

  path <- find_index(index)
  load_index(path)
}


#' @rdname load_index
#' @param index index file to re-save.
save_index <- function(index, quiet = FALSE) {
  stopifnot(is.index(index))
  stopifnot(!is.null(index$path))

  path <- index$path
  if (!quiet) message("Writing ", path)
  cat(format(index), "\n", file = path)
}

#' The index class.
#'
#' @keywords internal
index <- function(articles, path = NULL) {
  structure(list(articles = articles, path = path), class = "index")
}

#' @rdname index
is.index <- function(x) inherits(x, "index")

#' @S3method [ index
"[.index" <- function(x, i) {
  index(x$articles[i])
}

#' @S3method [[ index
"[[.index" <- function(x, i) {
  x$articles[[i]]
}

#' @S3method c index
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

#' @S3method length index
length.index <- function(x) length(x$articles)

#' @S3method format index
format.index <- function(x, ...) {
  articles <- lapply(x$articles, format)
  paste(articles, collapse = "\n\n")
}

#' @S3method print index
print.index <- function(x, ...) cat(format(x, ...), "\n")

