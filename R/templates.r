find_template <- function(name) {
  path <- system.file("templates", paste0(name, ".txt"), package = "rj")
  if (path == "") stop("Template not found", call. = FALSE)

  path
}

as.data <- function(x) {
  stopifnot(is.article(x))

  data <- lapply(x, format)
  data$name <- x$authors[[1]]$name
  data$email <- x$authors[[1]]$email
  data$editor <- Sys.getenv("RJ_NAME",
    unset = "Use RJ_NAME envname to set your name")

  data
}

#' @importFrom whisker whisker.render
render_template <- function(article, template) {
  article <- as.article(article)
  template <- find_template(template)

  whisker.render(readLines(template), as.data(article))
}
