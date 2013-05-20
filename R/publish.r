#' Publish an article to online first
#'
#' @importFrom tools texi2pdf
#' @examples
#' \dontrun{
#' arts <- accepted_articles()
#' for(art in arts) publish(art)
#' }
publish <- function(article, home = getwd()) {
  article <- as.article(article)

  # Make sure we're in the right place
  if (basename(home) != "articles") {
    stop("Publish should be run from articles directory", call. = FALSE)
  }
  web_path <- normalizePath("../web2", mustWork = TRUE)
  share_path <- normalizePath("../share", mustWork = TRUE)

  message("Publishing ", format(article$id))
  # Build latex and copy to new home
  build_latex(article)
  slug <- article$slug %||% make_slug(article$authors)

  from <- file.path(article$path, "RJwrapper.pdf")
  to <- file.path(web_path, "archive", "accepted", paste0(slug, ".pdf"))
  file.copy(from, to)
  message("Creating ", basename(to))

  article$slug <- slug
  update_status(article, "online")

  # Make yaml
  message("Remember to check changes into git")
  invisible(TRUE)
}

build_latex <- function(article) {
  article <- as.article(article)

  # Check RJournal.sty does not exist
  sty_path <- file.path(article$path, "RJournal.sty")
  if (file.exists(sty_path)) {
    stop("Article contains RJournal style file", call. = FALSE)
  }

  # Build latex
  in_dir(article$path,
    texi2pdf("RJwrapper.tex", texinputs = share_path, clean = TRUE)
  )
}


make_slug <- function(authors) {
  names <- unlist(lapply(authors, "[[", "name"))
  people <- as.person(names)
  family <- unlist(lapply(people, function(x) x[[1]]$family))
  if (length(family) > 3) {
    family <- c(family[1:3], "et-al")
  }
  paste0(family, collapse = "-")
}
