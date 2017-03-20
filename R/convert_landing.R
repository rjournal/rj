# function initially to convert 2013-1 thru 2016-2 to landing pages, extended
# for 2009-1 thru 2012-2

convert_proofs <- function(issue, action="report_only", clean=TRUE) {
  if (basename(getwd()) != "Proofs") stop("not in Proofs")
  if (!(issue %in% list.files())) stop("issue not found")
  yr_id <- str_sub(issue, 1L, 4L)
  share_path <- normalizePath("../../share", mustWork = TRUE)
  web_path <- normalizePath("../../rjournal.github.io", mustWork = TRUE)
  if (!dir.exists(file.path(web_path, "archive", yr_id)) 
    && action == "report_and_commit")
    dir.create(file.path(web_path, "archive", yr_id))
  current_dirs <- list.files(file.path(web_path, "archive", yr_id))
  i <- 0L
  if (length(current_dirs) > 0) {
    if (any(nchar(current_dirs) != 11L)) stop("landing dir-name length error")
  } else {
    i <- 1L
  }
  arts0 <- list.files(issue)
  arts <- arts0[!is.na(str_match(arts0, "^20[01][0-9]-"))]
  for (art in arts) {
    if (yr_id < "2013") {
      article <- list()
      from <- file.path(issue, art, "RJwrapper.pdf")
      if (!file.exists(from)) stop(from, " not found")
    } else {
      article <- as.article(file.path(issue, art))
    }
    if (!empty(article$slug)) {
      old_slug <- str_to_lower(article$slug)
      from <- file.path(web_path, "archive", issue, paste0(old_slug, ".pdf"))
      if (!file.exists(from)) stop(paste0(old_slug, ".pdf"), " not found")
    }
    if (!empty(article$slug)) {
      if (str_sub(article$slug, 1L, 5L) == "RJ-20") {
        if (str_sub(article$slug, 4L, 7L) < yr_id) {
          article$slug <- ""
        }
      } else {
        article$slug <- ""
      }
    }
    if (!empty(article$slug) &&
      str_sub(article$slug, 1L, 8L) == paste0("RJ-", yr_id, "-")) {
      this_dir <- str_sub(article$slug, 9L, 11L)
      if (this_dir %in% str_sub(current_dirs, 9L, 11L)) {
        if (!dir.exists(file.path(web_path, "archive",
          yr_id, article$slug))) {
          article$slug <- ""
        }
      } else {
        slug <- article$slug
      }
    }
    if (empty(article$slug)) {
      if (i == 0L) 
        i <- as.integer(max(as.integer(str_sub(current_dirs, 9L, 11L)))) + 1L
      next_dir <- formatC(i, format="d", flag="0", width=3L)
      slug <- paste0("RJ-", yr_id, "-", next_dir)
      if (action == "report_and_commit") 
        dir.create(file.path(web_path, "archive", yr_id, slug))
    } else{
      slug <- article$slug
    }
    landing_path <- file.path(web_path, "archive", yr_id, slug)

    cat(paste0(issue, ": article: ", art, ", yr_id: ", yr_id, ", slug: ", slug, "\n"))
    cat(paste0("  from: ", from, "\n"))
    cat(paste0("  landing_path: ", landing_path, "\n"))

    if (yr_id < "2013") {
      file.copy(from, file.path(issue, art, "oRJwrapper.pdf"))
      in_dir(file.path(issue, art),
        texi2pdf("RJwrapper.tex", texinputs = share_path, clean = clean))
      pdf_list <- get_md_from_pdf(from)
      file.copy(file.path(issue, art, "oRJwrapper.pdf"), from)

      cat(pdf_list$bibtitle, "\n")
      cat(pdf_list$bibauthor, "\n")
      cat(str_wrap(pdf_list$abstract, width=70), "\n")

      refs_list <- get_refs_from_tex(file.path(issue, art))
      if (!is.null(refs_list$CRANpkgs))
        refs_list$CTV_rev <- rev_dep_ctv(refs_list$CRANpkgs)

      print(refs_list)
       
      metadata <- c(list(
        title = pdf_list$title,
        bibtitle = pdf_list$bibtitle,
        slug = slug,
        author = pdf_list$author,
        bibauthor = pdf_list$bibauthor,
        abstract = pdf_list$abstract
        ), refs_list[!sapply(refs_list, is.null)])
      metadata <- c(metadata, list(landing = str_sub(slug, 4L, 7L)))

    } else {
      pdf_list <- get_md_from_pdf(file.path(issue, art, "RJwrapper.pdf"))

      cat(pdf_list$bibtitle, "\n")
      cat(pdf_list$bibauthor, "\n")
      cat(str_wrap(pdf_list$abstract, width=70), "\n")

      refs_list <- get_refs_from_tex(article$path)
      if (!is.null(refs_list$CRANpkgs))
        refs_list$CTV_rev <- rev_dep_ctv(refs_list$CRANpkgs)

      print(refs_list)
       
      sl <- as.data.frame(article$status)
      metadata <- c(list(
        title = pdf_list$title,
        bibtitle = pdf_list$bibtitle,
        slug = slug,
        author = pdf_list$author,
        bibauthor = pdf_list$bibauthor,
        abstract = pdf_list$abstract,
        acknowledged = format(sl$date[which(sl$status == "acknowledged")]),
        online = format(sl$date[max(which(sl$status == "online"))])

        ), refs_list[!sapply(refs_list, is.null)])
      metadata <- c(metadata, list(landing = str_sub(slug, 4L, 7L)))

    }

    
  }
  


}

rev_dep_ctv <- function(pkgs) {

  if (requireNamespace("ctv", quietly = TRUE)) {
    ctvs <- ctv::available.views()
    ctvs_dfl <- lapply(ctvs, function(x) data.frame(ctv=rep(x$name,
      length(x$packagelist$name)), pkg=x$packagelist$name,
      stringsAsFactors=FALSE))
    ctvs_df <- do.call("rbind", ctvs_dfl)
    t_ctvs_df <- tapply(ctvs_df$ctv, ctvs_df$pkg, c)
    res0 <- unname(unlist(t_ctvs_df[pkgs]))
    if (is.null(res0)) {
      res <- res0
    } else {
      res <- names(sort(table(res0), decreasing = TRUE))
    }
  } else {
    warning("install ctv for reverse dependencies")
  }
  res
}
