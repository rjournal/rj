globalVariables("titles")
# XXX: Repetition below. But I intend to split out the different
# sections in the future

print_submitted = function(latest) {
  art = latest[latest$status_status == "submitted", ]
  if (nrow(art) == 0) return(invisible(NULL))
  cli::cli_h1("Submitted ({nrow(art)})")
  titles = str_trunc(art$title, 50)
  items = glue::glue("{art$id} ({art$days_since_submission}): {titles}")
  cli_ul(items)
}

print_acknowledged = function(latest) {
  art = latest[latest$status_status == "acknowledged", ]
  if (nrow(art) == 0) return(invisible(NULL))
  cli::cli_h1(glue::glue("Acknowledged ({nrow(art)})"))
  titles = str_trunc(art$title, 50)
  items = glue::glue("{art$id} ({art$days_since_submission}): {titles}")
  cli_ul(items)
}

print_out_for_review = function(latest) {
  articles = latest[latest$status_status == "out for review", ]
  if (nrow(articles) == 0) return(invisible(NULL))
  cli::cli_h1("Out for Review ({nrow(articles)})")
  articles$title = stringr::str_trunc(articles$title, 50)
  cli::cli_ul()
  for (i in seq_len(nrow(articles))) {
    article = articles[i, ]
    item = glue::glue("{article$id} ({article$days_since_submission}): {article$title}")
    cli::cli_li(item)
    list_reviewers(article$id)
  }

  cli::cli_end()
}

#' @export
#' @importFrom cli cli_h1 cli_li cli_ul cli_end
#' @title Summarise intray
#' Summarise editors current in-tray
#' @param editor Editors initials. If \code{NULL}, looks for the
#' environment variable \code{RJ_EDITOR}.
summarise_articles = function(editor = NULL) {
  if (is.null(editor)) {
    editor = Sys.getenv("RJ_EDITOR")
  }
  all_articles = get_assignments(editor)
  latest = get_latest_assignments(all_articles)
  print_acknowledged(latest)
  print_submitted(latest)
  print_out_for_review(latest)
}

globalVariables("status_date")
#' @rdname summarise_articles
#' @export
get_assignments = function(editor) {
  grep_str = system2("find",
                     args = c(".", "-name", "DESCRIPTION", "-print",
                              "| xargs grep Editor",
                              "| grep", editor),
                     stdout = TRUE)
  path = stringr::str_remove(grep_str, "/DESCRIPTION:Editor: .*")
  id = stringr::str_remove(path, "^./(Rejected|Submissions)/")
  is_rejected = stringr::str_detect(grep_str, "Rejected/")
  id = id[!is_rejected]
  # assignments = tibble::tibble(id = id, is_rejected = is_rejected)
  # assignments = assignments[!assignments$is_rejected,]
  map(id, as.article) %>%
    map(~unpack_status(.x)) %>%
    map_df(rbind) %>%
    dplyr::arrange(id, status_date)
}

#' @importFrom purrr map_df map
#' @importFrom dplyr group_by filter ungroup mutate
get_latest_assignments = function(assignments) {
  assignments %>%
    dplyr::group_by(id) %>%
    dplyr::filter(status_date == max(status_date)) %>%
    dplyr::ungroup()
}

unpack_status = function(x) {
  id = x$id
  status = x$status
  purrr::map(status,
             ~tibble::tibble(id_year = id$year,
                             id_seq = id$seq,
                             id = paste0(id$year, "-", id$seq),
                             path = x$path,
                             title = x$title,
                             "status_date" = .x[[1]],
                             "status_status" = .x[[2]],
                             "status_comments" = .x[[3]])) %>%
    purrr::map_df(rbind) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(days_since_submission = as.numeric(Sys.Date() - min(status_date))) %>%
    dplyr::ungroup()
}
