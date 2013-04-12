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

invite_reviewers <- function(article, prefix = "1") {
  article <- as.article(article)
  for(i in seq_along(article$reviewers)) {
    invite_reviewer(article, i, prefix = prefix)
  }
}

invite_reviewer <- function(article, reviewer_id, prefix = "1") {
  article <- as.article(article)
  reviewer <- article$reviewers[[reviewer_id]]

  data <- as.data(article)
  data$email <- reviewer$email
  data$name <- reviewer$name
  data$date <- format(Sys.Date() + 30, "%d %b %Y")

  dest <- file.path(article$path, "correspondence")
  if (!file.exists(dest)) dir.create(dest)
  name <- paste0(prefix, "-invite-", reviewer_id, ".txt")

  template <- find_template("review")
  email <- whisker.render(readLines(template), data)
  writeLines(email, file.path(dest, name))

  email_text(email)
}
