#' S3 class for article objects
#'
#' Create or convert input into an s3 article object
#'
#' @details
#' if the article is not parsable, \code{article()} will
#' print an error message and return a unparsed blob. This ensures that
#' information is not lost even if some articles have parsing errors.
#'
#' Usually the best way to use \code{as_article()} is to have your working directory set to the
#' admin repo, and refer to articles by their id. See the examples section.
#'
#' @param ... Named arguments giving the components of an article:
#'   id, authors, title, editor, reviewers, status
#' @param quiet if \code{TRUE} suppresses failure messages.
#' @export
#' @rdname article
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
#' \dontrun{
#' as.article("2012-01")
#' as.article("2012-02")
#' }
#' @rdname article
as.article <- function(id) {
  if (is.article(id)) {
    return(id)
  }
  if (length(id) != 1) stop("Invalid ID")

  # Check to see if it's an existing directory
  if (file.exists(id) && is.dir(id)) {
    id <- file.path(id, "DESCRIPTION")
  }
  if (file.exists(id)) {
    path <- id
  } else {
    # Otherwise we use the articles root
    base <- c(
      "Rejected", "Accepted", "Submissions",
      file.path("Proofs", dir(file.path(get_articles_path(), "Proofs")))
    )
    pos <- file.exists(file.path(get_articles_path(), base, id, "DESCRIPTION"))

    if (sum(pos) == 0) stop("Can't find ", id, call. = FALSE)
    if (sum(pos) > 1) {
      path <- file.path(get_articles_path(), base[pos], id)
      if (sum(pos) == 2 && "Rejected" %in% basename(dirname(path))) {
        warning(id, " found in multiple locations, ignoring the Rejected folder copy.")
        pos[1] <- FALSE
      } else {
        stop(id, " found in multiple locations: ", paste(path, collapse=", "), call. = FALSE)
      }
    }
    path <- file.path(get_articles_path(), base[pos], id, "DESCRIPTION")
  }

  load_article(path)
}

load_article <- function(path, quiet = FALSE) {
  fields <- c("ID", "Slug", "Authors", "Keywords", "OtherIDs", "Title", "Editor", "AE", "Reviewers", "Status", "Suppl")
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
                         reviewers = "", status = "", path = "",type = "", suppl = "",
                         keywords = "", otherids = "") {
  structure(list(
    id = parse_id(id),
    other_id = otherids,
    slug = slug,
    suppl = parse_supplementaries(suppl),
    path = path,
    type = type,
    authors = parse_address_list(authors),
    keywords = str_c(keywords, sep = ", "),
    title = str_trim(title),
    editor = str_trim(editor),
    ae = str_trim(ae),
    reviewers = parse_address_list(reviewers),
    status = parse_status_list(status)
  ), class = "article")
}


parse_supplementaries <- function(suppl) {
  x <- str_trim(str_split(suppl, ",\\s*\n?")[[1]])
  x <- x[str_length(x) > 0]
  xs <- lapply(x, function(y) {
    class(y) <- "supplfile"
    y
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

  paste(
    "ID: ", format(x$id), "\n",
    if (!empty(x$other_id)) paste0("OtherIDs: ", x$other_id, "\n"),
    "Title: ", x$title, "\n",
    if (!empty(x$slug)) paste0("Slug: ", x$slug, "\n"),
    if (length(x$suppl)) paste0("Suppl:\n  ", paste(unlist(x$suppl), collapse=', '), "\n"),
    "Authors:", if (!empty(authors)) "\n  ", authors, "\n",
    if (!empty(x$keywords)) paste0("Keywords: ", x$keywords, "\n"),
    "Editor: ", x$editor, "\n",
    "AE: ", x$ae, "\n",
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

article_ids <- function(dirs = TRUE) {
  if(identical(dirs, TRUE)) {
    dirs <- c("Accepted", "Proofs", "Rejected", "Submissions")
  }
  c(
    # Articles not in proofs
    dir(file.path(get_articles_path(), setdiff(dirs, "Proofs")), pattern = "\\d{4}-\\d{2,}"),
    # Articles in proofs
    dir(
      # In all proofs...
      dir(file.path(get_articles_path(), intersect(dirs, "Proofs")), pattern = "\\d{4}-[1-4]", full.names = TRUE),
      # Search for articles
      pattern = "\\d{4}-\\d{2,}"
    )
  )
}

#' Tabulate article descriptions
#'
#' Produce a table describing articles in specific directories.
#'
#' @param dirs The directories containing articles to be searched and tabulated.
#'   If TRUE, then all directories will be searched.
#'
#' @export
tabulate_articles <- function(dirs = c("Accepted", "Submissions")) {
  # Find matching ids
  ids <- article_ids(dirs)

  # Read DESCRIPTION from ids and arrange in data frame.
  purrr::map_dfr(ids, tabulate_single)
}


tabulate_single <- function(id){
  art <- tryCatch(as.article(id), error = function(e) {
    stop(stringr::str_glue("Failed to parse the DESCRIPTION file of {id}: {e$message}"))
  })
  lst_to_tbl <- function(x) {
    x <- lapply(x, function(z) if(rlang::is_empty(z)) NA else z)
    tibble::as_tibble(x)
  }
  field_tbl <- function(field) {
    list(purrr::map_dfr(field, lst_to_tbl))
  }
  art$id <- format(art$id)
  art$suppl <- list(art$suppl)
  art$authors <- field_tbl(art$authors)
  art$reviewers <- field_tbl(art$reviewers)
  art$status <- field_tbl(art$status)
  tibble::new_tibble(art, nrow = 1)
}
