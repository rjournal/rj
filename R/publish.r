#' Publish an article to online first
#'
#' @importFrom tools texi2pdf
#' @examples
#' \dontrun{
#' arts <- accepted_articles()
#' for(art in arts) publish(art)
#' for(art in arts) bundle(art, "~/desktop")
#' for(art in arts) email_template(art, "copy-edit")
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
  build_latex(article, share_path)
  if (is.null(article$slug)) {
    names <- unlist(lapply(authors, "[[", "name"))
    slug <- make_slug(names)
  } else {
    slug <- article$slug
  }

  from <- file.path(article$path, "RJwrapper.pdf")
  to <- file.path(web_path, "archive", "accepted", paste0(slug, ".pdf"))
  file.copy(from, to, overwrite = TRUE)
  message("Creating ", basename(to))

  article$slug <- slug
  update_status(article, "online")

  # Make yaml
  message("Remember to check changes into git")
  invisible(TRUE)
}

build_latex <- function(article, share_path) {
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

#' @importFrom yaml as.yaml
accepted_metadata <- function() {
  articles <- accepted_articles()
  out <- lapply(articles, function(x) {
    names <- vapply(x$authors, function(x) {
      format(as.person(x), include = c("given", "family"))[[1]]
    }, FUN.VALUE = character(1))

    list(
      title = x$title,
      slug = x$slug,
      author = names
    )
  })
  as.yaml(out)
}

#' @export
make_slug <- function(names) {
  people <- as.person(names)
  family <- unlist(lapply(people, function(x) x[[1]]$family))
  if (length(family) > 3) {
    family <- c(family[1:3], "et-al")
  }

  family <- iconv(family, to = "ASCII//translit")
  family <- gsub("[^A-Za-z]", "", family)
  paste0(family, collapse = "-")
}

bundle <- function(article, dest_path) {
  article <- as.article(article)
  dest <- file.path(dest_path, paste0(format(article$id), ".zip"))

  # Check confidential files are present, but don't include in zip file
  files <- dir(article$path)
  conf <- c("DESCRIPTION", "correspondence")

  if (length(intersect(files, conf)) != 2) {
    stop(paste(setdiff(conf, files), collapse = ", "), "not found in ", article$path)
  }
  files <- setdiff(files, conf)

  # Create zip file
  in_dir(article$path, zip(dest, files))
}

