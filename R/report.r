#' Generate a status report.
#'
#' @param articles list of articles to generate report for. Defaults to
#'   all active reports in \file{Submissions/}.
#' @export
report <- function(articles = active_articles()) {
  rpt <- do.call("rbind", lapply(articles, report_line))
  rpt <- rpt[order(rpt$date, rpt$ed), ]
  rpt$status <- factor(rpt$status, order_status(rpt$status))
  structure(rpt, class = c("report", "data.frame"))
}

last_status <- function(x) {
  stopifnot(is.article(x))
  x$status[[length(x$status)]]
}

summary_status <- function(x) {
  stopifnot(is.article(x))

  status <- last_status(x)$status
  if (status %in% final_status) {
    "complete"
  } else if (empty(x$editor)) {
    "needs editor (editor-in-chief)"
  } else if (empty(x$reviewers)) {
    "needs reviewers (editor)"
  } else {
    todo(status)
  }
}

todo <- function(status) {
  switch(status,
    "major revision" = "waiting (author)",
    "minor revision" = "waiting (author)",
    "out for review" = "waiting (reviewers)",
    "updated" = "waiting (editor)",
    "accepted" = "needs proofing (author)",
    "proofed" = "needs online (editor-in-chief)",
    "online" = "needs copy-editing (editor)",
    "copy-edited" = "ready for publication (editor-in-chief)"
  )
}

report_line <- function(x) {
  stopifnot(is.article(x))

  sstatus <- summary_status(x)
  status <- last_status(x)
  last_date <- last_status(x)$date

  days_taken <- difftime(Sys.Date(), last_date, "days")
  stars <- sum(days_taken > deadlines(sstatus))

  data.frame(
    status = sstatus,
    ed = editor_abbr(x$editor),
    id = format(x$id),
    title = str_trunc(x$title, 34),
    date = last_date,
    stars = str_dup("*", stars),
    stringsAsFactors = FALSE
  )
}

#' @S3method print report
print.report <- function(x, ...) {

  cat("BY STATUS:\n")
  parts <- split(x, x$status)
  for (nm in names(parts)) {
    part <- parts[[nm]]

    str_sub(nm, 1, 1) <- toupper(str_sub(nm, 1, 1))
    cat(str_pad(nm, 60, "right", "-"), "\n")

    out <- capture.output(print.data.frame(part[, -1], row.names = FALSE,
      right = FALSE))
    cat(paste(out[-1], collapse = "\n"), "\n\n")
  }

  cat("BY EDITOR:\n")
  actionable <- subset(x, ed != "" &
    !(status %in% c("accepted", "online", "complete")))
  parts <- split(actionable, actionable$ed)
  for (nm in names(parts)) {
    part <- parts[[nm]]

    str_sub(nm, 1, 1) <- toupper(str_sub(nm, 1, 1))
    cat(str_pad(nm, 60, "right", "-"), "\n")

    out <- capture.output(print.data.frame(part[, c("id", "status", "date", "stars")], row.names = FALSE,
      right = FALSE))
    cat(paste(out[-1], collapse = "\n"), "\n\n")
  }
}

order_status <- function(x) {
  x <- unique(x)
  first <- intersect(c("needs editor", "needs reviewers", "out for review"), x)
  last <- intersect(c("accepted", "copy edited", "online", "proofed", "complete"), x)
  c(first, setdiff(x, c(first, last)), last)
}

# Takes a summary status as input, and returns number of days before it's due
deadlines <- function(sstatus) {
  if (sstatus %in% final_status) {
    return(c(Inf, Inf))
  }

  special <- list(
    "needs editor" = c(7L, 14L),
    "needs reviewers" = c(7L, 14L),
    "submitted" = c(3L, 7L),
    "proofed" = c(7L, 14L),
    "major revision" = c(60L, 90L)
  )
  if (sstatus %in% names(special)) {
    special[[sstatus]]
  } else {
    c(4L, 6L) * 7L
  }
}

editor_abbr <- function(x) {
  if (empty(x)) return("")
  toupper(str_c(str_sub(str_split(x, " ")[[1]], 1, 1), collapse = ""))
}
