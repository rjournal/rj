#' Make a proof of an issue
#'
#' The `make_proof()` function is the first step to creating an issue. It moves
#' the 'proofed' articles from the `Accepted` folder and news articles from
#' `News_items/{id}` into `Proofs/{id}`.
#'
#' After the proof is made with this function, `publish_issue()` can be used to
#' publish these articles into the `rjournal.github.io` repository.
#'
#' @param id The id of the issue to proof
#' @param exec Set to TRUE to make the proof, the default (FALSE) allows a preview of which articles will be moved where.
#'
#' @export
make_proof <- function(id, exec = FALSE) {
  old <- setwd(get_articles_path())
  on.exit(setwd(old))
  dir <- issue_dir(id)
  if (!file.exists(file.path(dir, "news"))) {
    xfun::dir_create(file.path(dir, "news"))
  }

  arts <- accepted_articles()
  ready <- filter_status(arts, "proofed")
  for (art in ready) {
    if (exec) {
      system(paste(
        "git mv",
        shQuote(art$path),
        shQuote(file.path(dir, format(art$id)))
      ))
    } else {
      cat(art$path, file.path(dir, format(art$id)), "\n")
    }
  }

  news <- news_articles(id)
  for (art in news) {
    if (exec) {
      file.copy(
        art,
        file.path(dir, "news"),
        recursive = TRUE
      )
    } else {
      cat(art, file.path(dir, "news", basename(art)), "\n")
    }
  }

  if (!exec) {
    cli_alert_info("If these articles look correct, re-run the function with `exec = TRUE` to execute the move.")
  }
}
