#' Generate a status report.
#'
#' This should be run weekly.
#'
#' @param articles list of articles to generate report for. Defaults to
#'   all active reports in \file{Submissions/}.
#' @export
report <- function(articles = active_articles()) {
  rpt <- do.call("rbind", lapply(articles, report_line))
  rpt <- rpt[order(rpt$date, rpt$ed), ]
  rpt$status <- factor(rpt$status, order_status(rpt$status))
  # Sort by editor, then status, then date
  rpt <- rpt[order(-nchar(rpt$stars), rpt$ed, rpt$status, rpt$date), ]
  structure(rpt, class = c("report", "data.frame"))
}

report_line <- function(x) {
  message(x$path) # to identify culprit when things go wrong
  stopifnot(is.article(x))

  todo <- todo(x)
  status <- last_status(x)
  last_date <- last_status(x)$date

  days_taken <- difftime(Sys.Date(), last_date, "days")
  if(todo == "waiting (author)") {
    stars <- 0
  } else {
    stars <- sum(days_taken > deadlines(todo))
  }

  data.frame(
    status = todo,
    ed = editor_abbr(x$editor),
    id = format(x$id),
    title = str_trunc(x$title, 34),
    date = last_date,
    stars = str_dup("*", stars),
    stringsAsFactors = FALSE
  )
}

last_status <- function(x) {
  stopifnot(is.article(x))
  x$status[[length(x$status)]]
}

has_status <- function(x, status) {
  stopifnot(is.article(x))
  status %in% vapply(x$status, `[[`, character(1L), "status")
}

#' @export
print.report <- function(x, ...) {
  cat("BY STATUS:\n")
  parts <- split(x, x$status)
  for (nm in names(parts)) {
    part <- parts[[nm]]

    str_sub(nm, 1, 1) <- toupper(str_sub(nm, 1, 1))
    cat(str_pad(nm, 60, "right", "-"), "\n")

    out <- capture.output(print.data.frame(part[, -1],
      row.names = FALSE,
      right = FALSE
    ))
    cat(paste(out[-1], collapse = "\n"), "\n\n")
  }

  cat("BY EDITOR:\n")
  actionable <- x[x$ed != "" &
    !(x$status %in% c("accepted", "online", "complete")), ]
  parts <- split(actionable, actionable$ed)
  for (nm in names(parts)) {
    part <- parts[[nm]]

    str_sub(nm, 1, 1) <- toupper(str_sub(nm, 1, 1))
    cat(str_pad(nm, 60, "right", "-"), "\n")

    out <- capture.output(print.data.frame(part[, c("id", "status", "date", "stars")],
      row.names = FALSE,
      right = FALSE
    ))
    cat(paste(out[-1], collapse = "\n"), "\n\n")
  }
}

order_status <- function(x) {
  x <- unique(x)
  eic <- x[grepl("editor-in-chief", x)]
  editors <- setdiff(x[grepl("editor", x)], eic)
  author <- x[grepl("author", x)]
  others <- setdiff(x, c(eic, editors, author))
  c(eic, editors, others, author)
}

editor_abbr <- function(x) {
  if (empty(x)) {
    return("")
  }
  toupper(str_c(str_sub(str_split(x, " ")[[1]], 1, 2), collapse = ""))
}
