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
    arts <- arts[if (is.logical(status)) status else arts$status == status, ]
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
    latest, .in_revision(latest$status),
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
  cond <- grepl("revision", latest$status) |
    (latest$status %in% c(
      "rejected", "submitted", "acknowledged",
      "with AE", "out for review"
    ))
  if (any(!cond)) {
    print_sum(latest, !cond, , "{art$id} {art$status} ({art$days_since_submission}): {art$title}",
      description = "Other"
    )
  }
}

print_unassigned <- function(unassigned){
  print_sum(unassigned, "acknowledged", description = "Submitted but not assigned")
}


#' Summarise editors current in-tray
#'
#' This function summarises and prints the articles an editor currently have in hand.
#' It also prints out the articles that has not been assigned to any editor on the top, if any.
#' If assigned to an object, the unassigned articles will appear on the top of the data frame.
#'
#' @param editor Editors initials. If \code{NULL}, looks for the
#' environment variable \code{RJ_EDITOR}.
#' @param rejected Default \code{FALSE}. If \code{TRUE}, show
#' rejected articles.
#' @param other Default \code{FALSE}. If \code{TRUE}, list all
#' articles not covered by any of the other options (typically
#' accepted and online)
#' @importFrom cli cli_h1 cli_li cli_ul cli_end
#' @importFrom dplyr group_by filter ungroup mutate
#' @importFrom purrr map_dfr
#' @export
summarise_articles <- function(editor = NULL,
                               rejected = FALSE, other = FALSE) {
  if (is.null(editor)) {
    editor <- Sys.getenv("RJ_EDITOR")
  }
  if (rejected) folder <- c("Submissions", "Rejected") else folder <- c("Submissions")
  all_articles <- get_assignments(editor, folder)
  latest <- get_latest(all_articles)
  all_articles$days_since_submission <- latest$days_since_submission
  unassigned <- get_unassigned()
  if (!is.null(unassigned)) {
    lastest_unassigned <- get_latest(unassigned)
    unassigned$days_since_submission <- lastest_unassigned$days_since_submission
    print_unassigned(lastest_unassigned)
  }
  if (isTRUE(rejected)) print_rejected(latest)
  print_acknowledged(latest)
  print_with_ae(latest)
  print_submitted(latest)
  print_out_for_review(latest)
  print_in_revision(latest)
  print_revision_received(latest)
  if (isTRUE(other)) print_other(latest)
  res <- rbind(unassigned, all_articles %>% dplyr::arrange(path))
  return(invisible(res))
}

#' @export
#' @title Show articles that require attention with the corresponding action
#' @param editor string, initial of an editor or an associate editor, defaults to \code{RJ_EDITOR} env var
#' @param invite integer, number of days after which an invite is considered overdue
#' @param review integer, number of days after which a review is considered overdue. Note that you can extend this by specifying a due date in the comment, e.g.: "2021-03-01 Agreed (until 2021-05-01)" would allow for two months.
#' @param verbose logical, if \code{TRUE} it will always list the full reviewer report for each entry
actionable_articles <- function(editor, invite = 7, review = 30, verbose = FALSE) {
  if (missing(editor)) {
    editor <- Sys.getenv("RJ_EDITOR")
  }
  all_articles <- get_assignments(editor)
  latest <- get_latest(all_articles)
  work <- latest$status %in% c("acknowledged", "submitted", "with AE")
  rev <- latest$status == "out for review"
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

#' @param folder the folder to search for assignments, one of Submissions, Rejected, Accepted, or Proofs
#' @rdname summarise_articles
#' @export
get_assignments <- function(editor, folder = "Submissions") {

  ae_initials <- read.csv(system.file("associate-editors.csv", package = "rj"))$initials
  editors_all <- read.csv(system.file("editors.csv", package = "rj"))$name
  # include only "DC" "MK" "CH" "CG" "SU" "GS"
  editor_initials <- editors_all[11:16]

  if (editor %in% ae_initials){
    role <- "AE"
  } else if (editor %in% editor_initials){
    role <- "Editor"
  } else {
    cli::cli_abort("{.field {editor}} is neither an editor or an associated editor (ae). ")
  }


  grep_str <- find_articles(editor, folder, role)
  path <- stringr::str_remove(grep_str, glue::glue("/DESCRIPTION:{role}: .*"))
  id <- stringr::str_remove(path, "^./(Rejected|Submissions)/")

  if (length(id) == 0) {
    cli::cli_abort("No article found under this (associate) editor.")
  }

  purrr::map_dfr(id, tabulate_single)
}

#' @rdname summarise_articles
#' @export
get_unassigned <- function(){
  grep_str <- find_articles("'\\s$'", "Submissions", "Editor")
  id <- stringr::str_remove(grep_str, "/DESCRIPTION:Editor:.")
  if (length(id) != 0) purrr::map_dfr(id, tabulate_single)
}

#' @param role string, take value of either "Editor" or "AE"
#' @rdname summarise_articles
find_articles <- function(editor, folder, role){

  grep_ae_or_editor <- glue::glue("| xargs grep {role}")

  suppressWarnings(
    system2("find",
            args = c(
              folder, "-name", "DESCRIPTION", "-print",
              grep_ae_or_editor,
              "| grep", editor),
            stdout = TRUE
    )
  )
}

#' @param assignments an output object from \code{get_assignments()} or \code{get_unassigned()}
#' @rdname summarise_articles
get_latest <- function(assignments) {
  assignments %>%
    tidyr::unnest(status) %>%
    dplyr::group_by(id) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::mutate(days_since_submission = as.numeric(Sys.Date() - min(date))) %>%
    dplyr::ungroup()
}
