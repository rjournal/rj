
email_text <- function(text) {
  stopifnot(is.character(text))
  text <- paste(text, collapse = "\n")

  pieces <- str_split_fixed(text, "-{3, }", n = 2)[1, ]
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

  url <- paste0("mailto:", to, "?",
    paste0(names(fields), "=", unlist(fields), collapse = "&"))

  browseURL(url)
}


email_template <- function(article, template) {
  article <- as.article(article)
  text <- render_template(article, template)
  email_text(text)
}
