library(base64enc)
library(RJSONIO)
library(httr)

default_key <- function () {
  key <- Sys.getenv("POSTMARKAPP_API_KEY")
  if (key == "") {
    stop("Either provide key or set envvar POSTMARKAPP_API_KEY", call. = FALSE)
  }
  key
}


pm_req <- function(url, ..., config = NULL, key = default_key()) {

  headers <- add_headers(
    "X-Postmark-Server-Token" = key,
    "Accept" = "application/json",
    "Content-Type" = "application/json"
  )

  resp <- POST(url, ..., config = c(config, headers))

  if (identical(resp$status_code, 200)) {
    return(invisible(TRUE))
  }

  con <- content(resp, as = "parsed")
  stop(con$Message, " [", con$ErrorCode, "]", call. = FALSE)
}

send_email <- function(to, from, subject, body, cc = NULL, bcc = NULL,
                       attachments = NULL) {

  attach <- lapply(attachments, attach_file)
  data <- list(
    to = to,
    cc = cc,
    bcc = bcc,
    from = from,
    textbody = body,
    subject = subject,
    attachments = attach
  )

  pm_req("https://api.postmarkapp.com/email", body = toJSON(data))
}

attach_file <- function(path, type = NULL) {
  list(
    Name = basename(path),
    ContentType = type %||% guess_media(path) %||% "application/octet-stream",
    Content = base64encode(path)
  )
}

"%||%" <- function(a, b) if (is.null(a)) b else a
