find_template <- function(name) {
  path <- system.file("templates", paste0(name, ".txt"), package = "rj")
  if (path == "") stop("Template not found: ", name, call. = FALSE)

  path
}

as.data <- function(x) {
  stopifnot(is.article(x))

  data <- lapply(x, format)
  data$name <- x$authors[[1]]$name
  data$email <- x$authors[[1]]$email
  if (!empty(x$editor)) data$editor <- editors[[x$editor]]
  data$me <- Sys.getenv("RJ_NAME",
    unset = "Use RJ_NAME envname to set your name")

  data
}

editors <- c(
  "Hadley Wickham" = "h.wickham@gmail.com",
  "Heather Turner" = "ht@heatherturner.net",
  "Martyn Plummer" = "Martyn.Plummer@r-project.org",
  "Deepayan Sarkar" = "deepayan.sarkar@r-project.org",
  "Bettina Grün" = "Bettina.Gruen@jku.at"
)

#' @importFrom whisker whisker.render
render_template <- function(article, template) {
  article <- as.article(article)
  template <- find_template(template)

  whisker.render(readLines(template), as.data(article))
}

invite_reviewers <- function(article, prefix = "1") {
  article <- as.article(article)
  update_status(article, "out for review")
  
  for(i in seq_along(article$reviewers)) {
    invite_reviewer(article, i, prefix = prefix)
  }
}

invite_reviewer <- function(article, reviewer_id, prefix = "1") {
  article <- as.article(article)

  dest <- file.path(article$path, "correspondence")
  if (!file.exists(dest)) dir.create(dest)

  reviewer <- article$reviewers[[reviewer_id]]
  name <- paste0(prefix, "-invite-", reviewer_id, ".txt")
  path <- file.path(dest, name)
  
  if (!file.exists(path)) {
    data <- as.data(article)
    data$email <- reviewer$email
    data$name <- reviewer$name
    data$date <- format(Sys.Date() + 30, "%d %b %Y")
  
    template <- find_template("review")
    email <- whisker.render(readLines(template), data)
    
    writeLines(email, path)
  } else {
    message("Already invited - resending")
    email <- paste0(readLines(path), collapse = "\n")
  }
  
  email_text(email)
}
