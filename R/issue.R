
# NM, Sept. 4:

# The function make_proof() creates Proofs/year-ed, creates the RJ-*.tex
# file etc. that serve as the basis of the issue, and moves the article
# files (the ones online) to Proofs/year-ed.  This then makes the latter
# directory a "staging area," for articles waiting to be proofed and
# eventually put into an issue.

# Accordingly, build_issue () excludes articles not yet proofed.

# NM, Sept. 17:

# The code tacitly assumes that any article put online will necessarily
# appear in the next issue.  Our postponement of some of the articles
# originally slated for Vol. 11, No. 1 means adjustments will need to be
# made, on a one-time basis.

make_proof <- function(id, share_path = file.path("..", "share"), exec=FALSE) {
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
