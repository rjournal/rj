# Parse articles in subdirectories of base.
# On error raises articleListError conditon which has
# all individual errors as a list in the `errors` element.
.parse.articles <- function(subpaths, base=get_articles_path()) {
    bases <- file.path(base, subpaths)
    paths <- grep("2\\d{3}-\\d{1,3}$", dir(bases, full.names=TRUE), value=TRUE)
    l <- lapply(paths, function(id) tryCatch(as.article(id),
                                             error=function(e) e))
    names(l) <- paths
    err <- sapply(l, inherits, "error")
    if (any(err))
        stop(errorCondition(
            paste0("The following directories had errors:\n", paste(names(l)[err], collapse="\n")),
            errors=l[err], class="articleErrorList"))
    l
}

#' List articles.
#'
#' List all articles in common directories.
#'
#' \itemize{
#'  \item \code{active_articles}: \file{Submissions/}, \file{Accepted/}
#'  \item \code{accepted_articles}: \file{Accepted/}
#' }
#' @param include A character vector of directories to include.
#' @export
active_articles <- function(include = c("Submissions", "Accepted"))
    .parse.articles(include)

#' @rdname active_articles
#' @export
accepted_articles <- function()
    .parse.articles("Accepted")

news_articles <- function(issue)
    .parse.articles("News_items")

#' Generate a new id value.
#'
#' Inspects submissions/, accepted/ and rejected to figure out which
#' id is next in sequence.
#'
#' @export
new_id <- function() {
  ids <- dir(file.path(get_articles_path(), c("Submissions", "Accepted", "Rejected")))
  ids <- lapply(ids, parse_id)

  this_year <- Filter(function(x) x$year == year(), ids)

  if (length(this_year) == 0) {
    id(year(), 1)
  } else {
    seqs <- vapply(this_year, function(x) x$seq, integer(1))
    id(year(), max(seqs) + 1L)
  }
}

#' Find articles with a given status.
#'
#' @param articles A vector of articles, as given by accepted_articles()
#' @param status The status you are looking for
#' @export
filter_status <- function(articles, status) {
  Filter(function(a) has_status(a, status), articles)
}

#' Get articles to go online
#'
#' Find the articles that are accepted but have not yet been
#' published to online
#'
#' @export
get_accepted_but_not_online <- function() {
  l <- accepted_articles()
  o <- filter_status(l, "online")
  l_id <- sub("Accepted/", "", sapply(l, '[[', 5))
  o_id <- sub("Accepted/", "", sapply(o, '[[', 5))
  l_id[!(l_id %in% o_id)]
}
