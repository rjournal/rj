last_status <- function(x) {
  stopifnot(is.article(x))

  x$status[[length(x$status)]]
}

summary_status <- function(x) {
  stopifnot(is.article(x))

  if (empty(x$editor)) return("needs editor")
  if (empty(x$reviewers)) return("needs reviewers")

  status <- last_status(x)$status
  if (status %in% final_status) {
    "complete"
  } else {
    status
  }
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

report <- function(x) {
  stopifnot(is.index(x))

  rpt <- do.call("rbind", lapply(x$articles, report_line))
  rpt <- rpt[order(rpt$date, rpt$ed), ]
  rpt$status <- factor(rpt$status, order_status(rpt$status))
  structure(rpt, class = c("report", "data.frame"))
}

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
  last <- intersect(c("accepted", "online", "complete"), x)
  c(first, setdiff(x, c(first, last)), last)
}

# Takes a summary status as input, and returns number of days before it's due
deadlines <- function(sstatus) {
  if (sstatus %in% final_status) {
    return(c(Inf, Inf))
  }

  special <- c(
    "needs editor" = c(7L, 14L),
    "needs reviewer" = c(7L, 14L),
    "submitted" = c(3L, 7L)
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
