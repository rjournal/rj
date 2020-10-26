#' Create a new article (S3 class)
#'
#' This function will always succeed: if the article is not parseable it will
#' print an error message and return a unparsed blob. This ensures that
#' information is not lost even if some articles have parsing errors.
#'
#' @param ... Named arguments giving the components of an article:
#'   id, authors, title, editor, reviewers, status
#' @param quiet if \code{TRUE} suppresses failure messages.
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

#' Convert input into an article.
#'
#' @param id a path to a DESCRIPTION, a path to a directory containing
#'   DESCRIPTION, or a article name, found in a sub-directory rejected,
#'   accepted or submissions
#' @export
#' @examples
#' # Usually the best way to use is to have your working directory set to the
#' # admin repo, and refer to articles by their id.
#' \dontrun{
#' as.article("2012-01")
#' as.article("2012-02")
#' }
as.article <- function(id) {
  if (is.article(id)) return(id)

  # Check to see if it's an existing directory
  if (file.exists(id) && is.dir(id)) {
    id <- file.path(id, "DESCRIPTION")
  }
  if (file.exists(id)) {
    path <- id
  } else {
    # Otherwise, assume we're in the admin directory
    base <- c("Rejected", "Accepted", "Submissions",
              file.path("Proofs", dir("Proofs")))
    pos <- file.exists(file.path(get_articles_path(), base, id))
    # Assume working from articles directory
    # pos <- file.exists(base, id)

    if (sum(pos) == 0) stop("Can't find ", id, call. = FALSE)
    if (sum(pos) > 1) stop(id, " found in multiple locations", call. = FALSE)

    path <- file.path(get_articles_path(), base[pos], id, "DESCRIPTION")
    # path <- file.path(base[pos], id, "DESCRIPTION")
  }

  load_article(path)
}

load_article <- function(path, quiet = FALSE) {
  fields <- c("ID", "Slug", "Authors", "Title", "Editor", "AE", "Reviewers", "Status", "Suppl")
  dcf <- read.dcf(path, fields = fields, keep.white = fields)
  if (nrow(dcf) != 1) stop("DCF parsing error: ", path, call. = FALSE)

  # Remove field names that keep.white incorrectly preserves
  for (field in fields) {
    dcf[, field] <- gsub(paste(field, ": ?", sep = ""), "", dcf[, field])
  }
  # Convert missing values to empty strings
  dcf[is.na(dcf)] <- ""
  colnames(dcf) <- tolower(colnames(dcf))

  dcf <- as.list(as.data.frame(dcf, stringsAsFactors = FALSE))
  # Only should be manually set in tests
  if (is.null(dcf$id) || identical(dcf$id, "")) {
    dcf$id <- basename(dirname(path))
  }
  dcf$path <- dirname(path)
  do.call(article, dcf)
}

is.article <- function(x) inherits(x, "article")

make_article <- function(id, slug = "", authors = "", title = "", editor = "", ae = "",
                         reviewers = "", status = "", path = "", suppl = "") {
  structure(list(
    id = parse_id(id),
    slug = slug,
    suppl = parse_supplementaries(suppl),
    path = path,
    authors = parse_address_list(authors),
    title = str_trim(title),
    editor = str_trim(editor),
    ae = str_trim(ae),
    reviewers = parse_address_list(reviewers),
    status = parse_status_list(status)), class = "article")
}


parse_supplementaries <- function(suppl) {
  x <- str_trim(str_split(suppl, "\n")[[1]])
  x <- x[str_length(x) > 0]
  xs <- lapply(x, function(y) {
    class(y) <- "supplfile"; y
  })
  class(xs) <- "supplfile_list"
  xs
}

format.supplfile_list <- function(x, ...) {
  suppls <- lapply(x, format)
  paste(suppls, collapse = ",\n  ")
}

format.supplfile <- function(x, ...) {
  paste0(x)
}

empty.supplfile_list <- function(x) length(x) == 0


#' @method format article
#' @export
format.article <- function(x, ...) {
  authors <- format(x$authors)
  reviewers <- format(x$reviewers)
  status <- format(x$status)
  if (!empty(x$suppl)) suppl <- format(x$suppl)

  paste(
    "Title: ", x$title, "\n",
    if (!empty(x$slug)) paste0("Slug: ", x$slug, "\n"),
    if (!empty(x$suppl)) paste0("Suppl:\n  ", suppl, "\n"),
    "Authors:", if (!empty(authors)) "\n  ", authors, "\n",
    "Editor: ", x$editor, "\n",
    "AE:", x$AE, "\n",
    "Reviewers:", if (!empty(reviewers)) "\n  ", reviewers, "\n",
    "Status: ", if (!empty(status)) "\n  ", status,
    sep = ""
  )
}

save_article <- function(article, quiet = FALSE) {
  stopifnot(is.article(article))
  stopifnot(!is.null(article$path))

  path <- file.path(article$path, "DESCRIPTION")
  writeLines(format(article), path, useBytes = TRUE)
  cli::cli_alert_info(paste("Updated", path))
  invisible(article)
}


#' @method print article
#' @export
print.article <- function(x, ...) cat(format(x), "\n")

unparsed <- function(...) {
  structure(list(...), class = c("unparsed", "article"))
}

#' @method format unparsed
#' @export
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
