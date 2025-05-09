#' Send an email template.
#'
#' Interpolate article values into a template, and create a new (unsent)
#' email in your default mail client.  The email is created using
#' \code{\link{browseURL}} and the mailto protocol, so it must be relatively
#' brief.
#'
#' @section Text format:
#' The template should be divided into header and body with \code{---}.
#' The header should contain fields and values separated by \code{:} -
#' only a limited
#'
#' @section Template parameters:
#' The templates use whisker to insert template values. These have the form
#' \code{\{\{field_name\}\}}. You can use any field from the description as
#' well as the following special fields:
#'
#' \itemize{
#'   \item name: the name of the first author
#'   \item email: email address of first author
#'   \item editor: name of editor
#'   \item me: your name, as determine by envvar \code{RJ_NAME}
#' }
#'
#'
#' @param article An article id, e.g. \code{"2013-01"}
#' @param template The name of a template (without extension) found
#'   in \code{inst/templates}.
#' @export
email_template <- function(article, template) {
  article <- as.article(article)
  text <- render_template(article, template)
  save_email(article, text)
  email_text(text)
}

# Save email to correspondence folder

save_email <- function(article, text) {
  # Create correspondence folder if necessary
  dir <- file.path(article$path, "correspondence")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  # Create filename using date and time in UTC
  file <- file.path(dir, paste0(format(Sys.time(), "%Y-%m-%d-%H-%m", tz="UTC"), "_email.txt"))
  writeLines(text, file)
}

#' Generate an email template.
#'
#' @param text character vector, text of the e-mail (including headers)
#' @param means string, one of "mailto", "browser" (both open mailto: URL),
#'              "show" (file.show), "edit" (file.edit), "open" (shell open) or
#'              "clip" (system clipboard). Defaults to RJ_EMAIL_TOOL environment
#'              variable.
email_text <- function(text, means = getOption("RJ_EMAIL_TOOL", "mailto")) {
  stopifnot(is.character(text))
  text <- paste(text, collapse = "\n")

  pieces <- strsplit(text, "---", fixed = TRUE)[[1L]]
  stopifnot(is.character(pieces), length(pieces) == 2)

  head <- pieces[1]
  body <- pieces[2]

  head_lines <- str_split(head, "\n")[[1]]
  head_lines <- head_lines[head_lines != ""]
  head_pieces <- str_split_fixed(head_lines, ": ", n = 2)

  fields <- setNames(as.list(head_pieces[, 2]), tolower(head_pieces[, 1]))
  to <- fields$to
  fields$to <- NULL

  fields$body <- str_trim(body)
  fields <- lapply(fields, URLencode, reserved = TRUE)

  url <- paste0(
    "mailto:", to, "?",
    paste0(names(fields), "=", unlist(fields), collapse = "&")
  )

  if (means == "mailto" || means == "browser") {
    if (is.function(getOption("browser"))) {
      cli::cli_alert_info(
        "You have setup a custom 'browser' function which may or may not work.
        If your e-mail doesn't open up ready to send, try
        {.code options(browser=Sys.getenv('R_BROWSER'))}")
    }
    return(browseURL(url))
  }
  tmp <- tempfile("mail", fileext = ".txt")
  writeLines(text, tmp)
  switch(means,
         show = file.show(tmp),
         edit = file.show(tmp),
         open = system(paste("open", shQuote(tmp))),
         clip = {
             if (length(grep("^darwin", R.Version()$os))) {
                 message("E-mail has been written to the macOS clipboard")
                 on.exit(unlink(tmp))
                 con <- pipe("pbcopy")
                 writeLines(text, con)
                 close(con)
             } else if (.Platform$OS.type == "windows") {
                 message("E-mail has been written to the Windows clipboard")
                 on.exit(unlink(tmp))
                 utils::writeClipboard(text, format = 1)
             } else {
                 on.exit(unlink(tmp))
                 if (
                   suppressWarnings(
                     system(paste("xsel -i -c <", shQuote(tmp)), ignore.stdout=TRUE, ignore.stderr=TRUE) != 0 &&
                     system(paste("xclip -selection clipboard <", shQuote(tmp)), ignore.stdout=TRUE, ignore.stderr=TRUE) != 0
                  ))
                   stop("Neither xclip not xsel works - please install either tool")
                 message("E-mail has been written to the X11 clipboard")
             }
         },
         {
             unlink(tmp)
             stop("Unknown RJ_EMAIL_TOOL, must be one of mailto, browser, show, edit, open or clip.")
         })
}

find_template <- function(name) {
  if (name == "major revision") {
    path <- system.file("templates/revision-major.txt", package = "rj")
  } else if (name == "minor revision") {
    path <- system.file("templates/revision-minor.txt", package = "rj")
  } else {
    path <- system.file("templates", paste0(name, ".txt"), package = "rj")
  }
  if (path == "") stop("Template not found: ", name, call. = FALSE)

  path
}

guess_real_name <- function() {
  Sys.getenv("RJ_NAME",
    unset = if (.Platform$OS.type == "unix") {
      login <- Sys.info()[["login"]]
      finger <- system(paste("finger", login), intern = TRUE)
      sub(".*Name: ", "", finger[1L])
    } else {
      "Use RJ_NAME envvar to set your name"
    }
  )
}

as.data <- function(x) {
  stopifnot(is.article(x))

  data <- lapply(x, format)
  data$name <- x$authors[[1]]$name
  data$email <- x$authors[[1]]$email
  if (!empty(x$editor)) data$editor <- editors(TRUE)[x$editor]
  data$me <- guess_real_name()

  data
}

# @importFrom utils read.csv
# @param by.name logical, if \code{TRUE} then names are names and
#     values e-mails, otherwise names are e-mails and values names
editors <- function(by.name=FALSE) {
  cli::cli_alert_info("Reading inst/editor.csv")
  fname <- system.file("editors.csv", package = "rj")
  out <- read.csv(fname, stringsAsFactors = FALSE)
  #  out <- read.csv("editors.csv", stringsAsFactors = FALSE)
  if (by.name)
      `names<-`(out[["email"]], out[["name"]])
  else
      `names<-`(out[["name"]], out[["email"]])
}

#' @importFrom whisker whisker.render
render_template <- function(article, template) {
  article <- as.article(article)
  template <- find_template(template)

  whisker.render(readLines(template), as.data(article))
}
