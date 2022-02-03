globalVariables("titles")
# XXX: Repetition below. But I intend to split out the different
# sections in the future

## capitalises text except for and/for in the middle
smart.cap <- function(x) {
  unlist(lapply(strsplit(x, " ", TRUE), function(x) {
    x <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, 999))
    x <- paste(x, collapse = " ")
    gsub(" And ", " and ", gsub(" For ", " for ", x))
  }))
}

## This is the work-horse for all printing. It uses a template for
## each entry and optionally calls \code{detail(art$id)} to print
## additional details for each article, currently it is typically
## just list_reviewers
print_sum <- function(arts, status, detail = NULL,
                      glue = "{art$id} ({art$days_since_submission}): {art$title}",
                      description = smart.cap(status), pre.width = 20) {
    arts <- arts[if (is.logical(status)) status else arts$status_status == status, ]
    if (nrow(arts) == 0)
        return(invisible(NULL))

    cli::cli_h1("{description} {nrow(arts)}")
    arts$title <- str_trunc(arts$title, getOption("width") - pre.width)
    cli::cli_ul()
    ## sadly, cannot vectorise, because cli_lu screws up
    ## the format for simple output as in acknowledged.
    ## Also cli always calls glue with fixed options,
    ## so there is no way to override it (really bad design)
    ## or provide direct (non-glue) output.
    for (i in seq_len(nrow(arts))) {
        art <- arts[i, ]
        cli::cli_li(glue)
        if (is.function(detail)) detail(art$id)
        cli::cli_end()
    }
}

print_rejected <- function(latest) {
  print_sum(latest, "rejected", "{art$id}: {art$title}")
}

print_submitted <- function(latest) {
  print_sum(latest, "submitted")
}

print_acknowledged <- function(latest) {
  print_sum(latest, "acknowledged")
}

print_with_ae <- function(latest) {
  print_sum(latest, "with AE", ,
    "{art$id} ({art$days_since_submission}, {art$ae}): {art$title}",
    pre.width = 45
  )
} ## extra space for AE name

print_out_for_review <- function(latest) {
  print_sum(latest, "out for review", list_reviewers)
}

.in_revision <- function(status) {
  grepl("revision", status) &
    (status != "revision received")
}

## this covers both minor and major revision status as well as AE: variants
print_in_revision <- function(latest) {
  print_sum(
    latest, .in_revision(latest$status_status),
    list_reviewers, , "In Revision"
  )
}

print_revision_received <- function(latest) {
  print_sum(latest, "revision received", list_reviewers)
}

## this is just a a catch-all maintained manually to catch any unwanted
## entries. Note that if you add cases above, you must also modify this
## function, it is not maintained automatically.
print_other <- function(latest) {
  cond <- grepl("revision", latest$status_status) |
    (latest$status_status %in% c(
      "rejected", "submitted", "acknowledged",
      "with AE", "out for review"
    ))
  if (any(!cond)) {
    print_sum(latest, !cond, , "{art$id} {art$status_status} ({art$days_since_submission}): {art$title}",
      description = "Other"
    )
  }
}

#' @export
#' @importFrom cli cli_h1 cli_li cli_ul cli_end
#' @title Summarise intray
#' Summarise editors current in-tray
#' @param editor Editors initials. If \code{NULL}, looks for the
#' environment variable \code{RJ_EDITOR}.
#' @param rejected Default \code{FALSE}. If \code{TRUE}, show
#' rejected articles.
#' @param other Default \code{FALSE}. If \code{TRUE}, list all
#' articles not covered by any of the other options (typically
#' accepted and online)
summarise_articles <- function(editor = NULL,
                               rejected = FALSE, other = FALSE) {
  if (is.null(editor)) {
    editor <- Sys.getenv("RJ_EDITOR")
  }
  all_articles <- get_assignments(editor)
  latest <- get_latest_assignments(all_articles)
  if (isTRUE(rejected)) print_rejected(latest)
  # get only most recent status
  latest <- latest %>%
    group_by(id) %>%
    dplyr::slice_tail(n = 1)
  print_acknowledged(latest)
  print_with_ae(latest)
  print_submitted(latest)
  print_out_for_review(latest)
  print_in_revision(latest)
  print_revision_received(latest)
  if (isTRUE(other)) print_other(latest)
  return(invisible(all_articles))
}

#' @export
#' @title Show articles that require attention with the corresponding action
#' @param editor string, editor, defaults to \code{RJ_EDITOR} env var
#' @param invite integer, number of days after which an invite is considered overdue
#' @param review integer, number of days after which a review is considered overdue. Note that you can extend this by specifying a due date in the comment, e.g.: "2021-03-01 Agreed (until 2021-05-01)" would allow for two months.
#' @param verbose logical, if \code{TRUE} it will always list the full reviewer report for each entry
actionable_articles <- function(editor, invite = 7, review = 30, verbose = FALSE) {
  if (missing(editor)) {
    editor <- Sys.getenv("RJ_EDITOR")
  }
  all_articles <- get_assignments(editor)
  latest <- get_latest_assignments(all_articles)
  work <- latest$status_status %in% c("acknowledged", "submitted")
  rev <- latest$status_status == "out for review"
  cli::cli_h1(paste("Actionable articles"))
  cli_ul()
  for (id in unique(latest$id[rev | work])) {
    art <- as.article(id)
    rev <- reviewer_status(id)
    issues <- character()
    ## less than 2 reviews finished?
    if (sum(rev$fin) < 2) {
      # print(rev)
      inv <- rev[rev$st == "invited" | rev$st == "agreed" | rev$st == "revision", ]
      for (i in seq_len(nrow(inv))) {
        with(inv[i, ], {
          since <- as.numeric(Sys.Date() - date)
          expected <- if (st == "invited") invite else review
          if (ext > 0) expected <- ext
          if (since > expected) {
            issues <<- c(issues, paste0(
              "r", rid, ": ",
              if (st == "invited") "invite" else "review",
              " overdue (", since, " days, expected ", expected, "): ",
              name, " <", email, "> "
            ))
          }
        })
      }
      if (nrow(rev) < 2) {
        issues <- c(issues, paste0("Needs additional reviewer(s), has ", nrow(rev), " of 2"))
      }
    }
    if (length(issues)) {
      art$title <- str_trunc(art$title, getOption("width") - 20)
      cli_li()
      cli_li(paste0(id, " ", art$title))
      cli_ul()
      cli_li(issues)
      cli_end()
      if (verbose) {
        list_reviewers(id)
      }
      cli_end()
    }
  }
  cli_end()
}

globalVariables("status_date")
#' @rdname summarise_articles
#' @export
get_assignments <- function(editor) {
  suppressWarnings(
    grep_str <- system2("find",
                        args = c(
                          ".", "-name", "DESCRIPTION", "-print",
                          "| xargs grep Editor",
                          "| grep", editor
                        ),
                        stdout = TRUE
    )
  )

  path <- stringr::str_remove(grep_str, "/DESCRIPTION:Editor: .*")
  id <- stringr::str_remove(path, "^./(Rejected|Submissions)/")

  if (length(id) == 0) {
    cli::cli_abort("Editor not detected. Please enter the correct editor initial.")
  }

  map(id, as.article) %>%
    map(~ unpack_status(.x)) %>%
    map_df(rbind) %>%
    dplyr::arrange(id, status_date)
}

#' @importFrom purrr map_df map
#' @importFrom dplyr group_by filter ungroup mutate
get_latest_assignments <- function(assignments) {
  assignments %>%
    dplyr::group_by(id) %>%
    dplyr::filter(status_date == max(status_date)) %>%
    dplyr::ungroup()
}

unpack_status <- function(x) {
  id <- x$id
  status <- x$status
  purrr::map(
    status,
    ~ tibble::tibble(
      id_year = id$year,
      id_seq = id$seq,
      id = format(id),
      path = x$path,
      ae = format(x$ae),
      title = x$title,
      "status_date" = .x[[1]],
      "status_status" = .x[[2]],
      "status_comments" = .x[[3]]
    )
  ) %>%
    purrr::map_df(rbind) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(days_since_submission = as.numeric(Sys.Date() - min(status_date))) %>%
    dplyr::ungroup()
}
