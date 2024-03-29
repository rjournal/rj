# function initially to convert 2013-1 thru 2016-2 to landing pages, extended
# for 2009-1 thru 2012-2

convert_proofs <- function(issue, action = "report_only", clean = TRUE) {
  if (basename(getwd()) != "Proofs") stop("not in Proofs")
  if (!(issue %in% list.files())) stop("issue not found")
  yr_id <- str_sub(issue, 1L, 4L)
  share_path <- normalizePath("../../share", mustWork = TRUE)
  web_path <- normalizePath("../../rjournal.github.io", mustWork = TRUE)

  config <- yaml::yaml.load_file(file.path(web_path, "_config.yml"))
  issues <- sapply(config$issues, function(x) x$issue)
  this_issue <- which(issues == issue)
  if (length(this_issue) == 0L) stop("issue not in _config.yml")
  conf_articles <- config$issues[[this_issue]]$articles
  has_slug <- sapply(conf_articles, function(x) !is.null(x$slug))
  has_slug_ptr <- character(length(has_slug))
  has_slug_ptr[has_slug] <- sapply(conf_articles[has_slug], function(x) x$slug)

  if (!dir.exists(file.path(web_path, "archive", yr_id)) &&
    action == "report_and_commit") {
    dir.create(file.path(web_path, "archive", yr_id))
  }
  i <- 0L

  if (yr_id < "2013") {
    fls0 <- list.files(file.path(web_path, "archive", issue))
    fls <- fls0[str_detect(fls0, "^RJournal_")]
    strings <- "[A-Z][a-z][a-z]*"
    nms0 <- str_subset(str_replace_all(
      str_sub(fls, 17L, str_length(fls) - 4L),
      "[_\\+\\~]", " "
    ), strings)
    nms <- str_replace(nms0, " et al", "")
  }

  arts0 <- list.files(issue)
  arts <- arts0[!is.na(str_match(arts0, "^20[01][0-9]-[0-9][0-9][0-9]*"))]

  for (art in arts) {
    current_dirs <- list.files(file.path(web_path, "archive", yr_id))
    if (length(current_dirs) > 0) {
      if (any(nchar(current_dirs) != 11L)) stop("landing dir-name length error")
    }
    if (action != "report_and_commit" && i == 0) {
      if (length(current_dirs) > 0) i <- length(current_dirs)
    }

    if (yr_id < "2013") {
      article <- list()
      localfrom <- file.path(issue, art, "RJwrapper.pdf")
      if (!file.exists(localfrom)) stop(localfrom, " not found")
    } else {
      article <- as.article(file.path(issue, art))
      if (empty(article$slug)) stop("no slug in ", issue, " ", art)
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
        if (!dir.exists(file.path(
          web_path, "archive",
          yr_id, article$slug
        ))) {
          article$slug <- ""
        }
      } else {
        slug <- article$slug
      }
    }
    if (empty(article$slug)) {
      if (action != "report_and_commit") {
        i <- i + 1L
      } else {
        i <- ifelse(length(current_dirs) > 0,
          as.integer(max(as.integer(str_sub(current_dirs, 9L, 11L)))) + 1L, 1L
        )
      }
      next_dir <- formatC(i, format = "d", flag = "0", width = 3L)
      slug <- paste0("RJ-", yr_id, "-", next_dir)
      if (action == "report_and_commit") {
        dir.create(file.path(web_path, "archive", yr_id, slug))
      }
    } else {
      slug <- article$slug
    }
    landing_path <- file.path(web_path, "archive", yr_id, slug)
    cat(paste0("landing_path: ", landing_path, "\n"))
    if (!dir.exists(landing_path) && action == "report_and_commit") {
      dir.create(landing_path)
    }

    #    cat(paste0(issue, ": article: ", art, ", yr_id: ", yr_id, ", slug: ", slug, "\n"))
    #    cat(paste0("  from: ", from, "\n"))
    #    cat(paste0("  landing_path: ", landing_path, "\n"))

    if (yr_id < "2013") {
      file.copy(localfrom, file.path(issue, art, "oRJwrapper.pdf"))
      t0 <- try(in_dir(
        file.path(issue, art),
        tools::texi2pdf("RJwrapper.tex", texinputs = share_path, clean = clean)
      ))
      pdf_list <- get_md_from_pdf(localfrom)
      file.copy(file.path(issue, art, "oRJwrapper.pdf"), localfrom)

      #      cat(pdf_list$bibtitle, "\n")
      #      cat(pdf_list$bibauthor, "\n")
      #      cat(str_wrap(pdf_list$abstract, width=70), "\n")

      refs_list <- get_refs_from_tex(file.path(issue, art))
      if (!is.null(refs_list$CRANpkgs)) {
        refs_list$CTV_rev <- rev_dep_ctv(refs_list$CRANpkgs)
      }

      #      print(refs_list)

      metadata <- c(list(
        title = pdf_list$title,
        bibtitle = pdf_list$bibtitle,
        slug = slug,
        author = pdf_list$author,
        bibauthor = pdf_list$bibauthor,
        abstract = pdf_list$abstract
      ), refs_list[!sapply(refs_list, is.null)])
      metadata <- c(metadata, list(landing = str_sub(slug, 4L, 7L)))

      aus <- unlist(str_split(paste(metadata$author, collapse = " "), " "))
      if ("R\xc3\xb6nneg\xc3\xa5rd" %in% aus) {
        aus <- str_replace(str_replace(aus, "[\xc3\xb6]", "oe"), "[\xc3\xa5]", "aa")
      }
      if ("S\xc3\xb3lymos" %in% aus) {
        aus <- str_replace(aus, "[\xc3\xb3]", "o")
      }
      if ("Herv\xc3\xa9" %in% aus) {
        aus <- str_replace(aus, "[\xc3\xa9]", "e")
      }
      if ("B\xc3\xa5\xc3\xa5th" %in% aus) {
        aus <- str_replace_all(aus, "[\xc3\xa5]", "aa")
      }
      pub_file <- NULL
      if (issue == "2009-2" && art == "2009-07") aus <- "Coeurjolly"
      if (issue == "2012-2" && art == "2011-22") {
        pub_file <- "RJournal_2012-2_Murrell+Ly.pdf"
      }
      if (issue == "2012-2" && art == "2011-32") {
        pub_file <- "RJournal_2012-2_Murrell.pdf"
      }
      if (issue == "2012-2" && art == "2011-35") {
        pub_file <- "RJournal_2012-2_Murrell2.pdf"
      }

      if (is.null(pub_file)) {
        pub_file <- fls[which(sapply(
          lapply(
            nms,
            function(x) match(unlist(str_split(x, " ")), aus)
          ),
          function(y) any(!is.na(y))
        ))]
      }

      if (length(pub_file) == 0) {
        cat(nms, "\n")
        cat(metadata$author, collapse = " ", "\n")
        stop(issue, " ", art, " no match")
      }

      #      cat(issue, art, paste(metadata$author, collapse=" "), "\n")
      #      cat(pub_file, "\n")

      from <- file.path(web_path, "archive", issue, pub_file)
      if (!file.exists(from)) {
        stop("published file :", pub_file, "not found in ", issue)
      }

      metadata <- c(metadata, list(old_slug = str_sub(
        pub_file, 17L,
        str_length(pub_file) - 4L
      )))
      #      cat(from, "\n")
    } else {
      pdf_list <- get_md_from_pdf(file.path(issue, art, "RJwrapper.pdf"))

      #      cat(pdf_list$bibtitle, "\n")
      #      cat(pdf_list$bibauthor, "\n")
      #      cat(str_wrap(pdf_list$abstract, width=70), "\n")

      refs_list <- get_refs_from_tex(article$path)
      if (!is.null(refs_list$CRANpkgs)) {
        refs_list$CTV_rev <- rev_dep_ctv(refs_list$CRANpkgs)
      }

      #      print(refs_list)

      sl <- as.data.frame(article$status)
      metadata <- c(list(
        title = pdf_list$title,
        bibtitle = pdf_list$bibtitle,
        slug = slug,
        old_slug = old_slug,
        author = pdf_list$author,
        bibauthor = pdf_list$bibauthor,
        abstract = pdf_list$abstract,
        acknowledged = format(sl$date[which(sl$status == "acknowledged")]),
        online = format(sl$date[max(which(sl$status == "online"))])
      ), refs_list[!sapply(refs_list, is.null)])
      metadata <- c(metadata, list(landing = str_sub(slug, 4L, 7L)))
      #      cat(from, "\n")
    }
    conf_art <- which(has_slug_ptr == metadata$old_slug)
    #    cat(has_slug_ptr, "\n")
    #    cat(metadata$old_slug, "\n")
    if (length(conf_art) == 0L) stop("config slug mismatch")

    cat("ptr: ", conf_art, " derived slug: ", metadata$old_slug, "\n")
    cat(metadata$title, "\n")
    cat(conf_articles[[conf_art]]$title, "\n")
    cat(metadata$author, "\n")
    cat(conf_articles[[conf_art]]$author, "\n")
    cat(from, "\n")
    print(names(metadata))

    if (action == "report_and_commit") {
      article_landing <- make_landing(metadata, issue)
      writeLines(article_landing, file.path(landing_path, "index.html"))
      message("Creating landing page")

      to <- file.path(landing_path, paste0(slug, ".pdf"))
      file.copy(from, to, overwrite = TRUE)
      message("Creating ", basename(to))
    }

    article_config_content <- list(
      slug = metadata$slug,
      old_slug = metadata$old_slug,
      title = metadata$title,
      bibtitle = metadata$bibtitle,
      author = metadata$author,
      bibauthor = metadata$bibauthor,
      landing = metadata$landing,
      abstract = metadata$abstract
    )
    if (!is.null(conf_articles[[conf_art]]$pages)) {
      article_config_content <- c(
        article_config_content,
        list(pages = conf_articles[[conf_art]]$pages)
      )
    }
    if (!is.null(metadata$acknowledged)) {
      article_config_content <- c(
        article_config_content,
        list(acknowledged = metadata$acknowledged)
      )
    }
    if (!is.null(metadata$online)) {
      article_config_content <- c(
        article_config_content,
        list(online = metadata$online)
      )
    }
    if (!is.null(metadata$CRANpkgs)) {
      article_config_content <- c(
        article_config_content,
        list(CRANpkgs = metadata$CRANpkgs)
      )
    }
    if (!is.null(metadata$BIOpkgs)) {
      article_config_content <- c(
        article_config_content,
        list(BIOpkgs = metadata$BIOpkgs)
      )
    }
    if (!is.null(metadata$CTVs)) {
      article_config_content <- c(
        article_config_content,
        list(CTVs = metadata$CTVs)
      )
    }
    if (!is.null(metadata$CTV_rev)) {
      article_config_content <- c(
        article_config_content,
        list(CTV_rev = metadata$CTV_rev)
      )
    }

    print(str(article_config_content))

    if (action == "report_and_commit") {
      conf_articles[[conf_art]] <- article_config_content
    }
  }

  if (action == "report_and_commit") {
    config$issues[[this_issue]]$articles <- conf_articles
    writeLines(as.yaml(config), file.path(web_path, "_config.yml"))
  }
  invisible(TRUE)
}


rev_dep_ctv <- function(pkgs) {
  if (requireNamespace("ctv", quietly = TRUE)) {
    ctvs <- ctv::available.views()
    ctvs_dfl <- lapply(ctvs, function(x) {
      data.frame(
        ctv = rep(
          x$name,
          length(x$packagelist$name)
        ), pkg = x$packagelist$name,
        stringsAsFactors = FALSE
      )
    })
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
