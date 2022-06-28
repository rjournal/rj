#' Publish an article to online first.
#'
#' After running this, you'll need to update both the article and
#' the web repository.
#'
#' @export
#' @importFrom tools texi2pdf
#' @importFrom yaml yaml.load_file
#' @param article article id
#' @param home Location of the articles directory
#' @param legacy (Very) old way of referening the R journal
publish_article <- function(article, volume, issue, home = get_articles_path(), legacy = FALSE) {
  cli::cli_alert_info("Publishing article {article}")
  article <- as.article(article)

  # Make sure we're in the right place
  if (basename(home) != "articles") {
    stop("Publish should be run from articles directory", call. = FALSE)
  }
  web_path <- normalizePath("../rjournal.github.io", mustWork = TRUE)
  share_path <- normalizePath("../share", mustWork = TRUE)

  if (!any(as.data.frame(article$status)$status == "accepted")) {
    stop("not yet accepted")
  }
  # if (!any(as.data.frame(article$status)$status == "style checked"))
  #  stop("not yet style checked")

  message("Publishing ", format(article$id))
  # # Build latex
  # if (!file.exists(file.path(article$path, "RJwrapper.pdf"))) {
  #   build_latex(article, share_path)
  # }
  # from <- file.path(article$path, "RJwrapper.pdf")

  if (legacy) {
    # create slug if needed and path to new home
    if (empty(article$slug) || str_sub(article$slug, 1L, 5L) == "RJ-20") {
      names <- unlist(lapply(article$authors, "[[", "name"))
      slug <- make_slug(names)
    } else {
      slug <- article$slug
    }
    # to <- file.path(web_path, "archive", "accepted", paste0(slug, ".pdf"))
  } else {
    # Create slug and stage new YYYY/RJ-YYYY-XXX landing directory
    yr_id <- format(Sys.Date(), "%Y")
    slug_pattern <- "^RJ-(\\d{4})-(\\d{3})$"
    current_dirs <- list.files(file.path(web_path, "_articles"), pattern = paste0("^RJ-", yr_id, "-", "\\d{3}"))

    # Remove slug if existing slug has wrong format or year
    if (!empty(article$slug)) {
      slug_year <- sub(slug_pattern, "\\1", article$slug)
      if(!grepl(slug_pattern, article$slug)) {
        article$slug <- ""
      }
    }
    # Validate existing slug exists and is used
    if (!empty(article$slug)) {
      if(!dir.exists(file.path(web_path, "_articles", article$slug))) {
        article$slug <- ""
      }
    }
    # If the slug doesn't exist, create it.
    all_slugs <- dir(
      file.path(web_path, "_articles"),
      pattern = paste0("^RJ-", yr_id, "-\\d{3}$")
    )
    if (empty(article$slug)) {
      next_id <- if(length(all_slugs) > 0) {
        max(as.integer(sub(slug_pattern, "\\2", all_slugs))) + 1L
      } else {
        1L
      }
      article$slug <- paste0("RJ-", yr_id, "-", formatC(next_id, format = "d", flag = "0", width = 3L))
      dir.create(file.path(web_path, "_articles", article$slug))
    }
    slug <- article$slug

    if (!dir.exists(file.path(web_path, "_articles", slug))) {
      dir.create(file.path(web_path, "_articles", slug))
    }

    landing_path <- file.path(web_path, "_articles", slug)
    # to <- file.path(landing_path, paste0(slug, ".pdf"))
  }

  # Remove dir if publish fails
  on.exit({
    if(!file.exists(file.path(landing_path, xfun::with_ext(slug, ".Rmd")))) {
      unlink(landing_path, recursive = TRUE)
    }
  })

  # # copy PDF to target
  # file.copy(from, to, overwrite = TRUE)
  message("Creating ", slug)

  if (!empty(article$suppl)) {
    if (any(missing_suppl <- !file.exists(file.path(article$path, unlist(article$suppl))))) {
      cli::cli_abort(
        c(
          "Supplementary file(s) not found for article {basename(article$path)}",
          setNames(unlist(article$suppl)[missing_suppl], "*")
        )
      )
    }
    zipfrom <- file.path(article$path, "supplementaries.zip")
    ret <- zip(zipfrom, file.path(article$path, unlist(article$suppl)),
               flags = "-j"
    )
    if (ret != 0L) stop("zipfile creation error")
    zipto <- file.path(landing_path, paste0(slug, ".zip"))
    file.copy(zipfrom, zipto, overwrite = TRUE)
    message("Creating ", basename(zipto))
  }

  rmd_path <- move_article_web(article$path, landing_path, volume, issue)
  article <- update_status(article, "online")
  rmarkdown::render(rmd_path, envir = new.env(), quiet = TRUE)

  # message("Remember to check changes into git")
  invisible(TRUE)
}

publish_news <- function(news, volume, issue, home = get_articles_path()) {
  cli::cli_alert_info("Publishing news article {basename(news)}")
  web_news_dir <- normalizePath(file.path(home, "..", "rjournal.github.io", "_news"))
  news_slug <- paste0("RJ-", 2008+volume, "-", issue, "-", basename(news))
  rmd_path <- move_article_web(news, file.path(web_news_dir, news_slug), volume, issue)
  rmarkdown::render(rmd_path, envir = new.env(), quiet = TRUE)
}

publish_issue <- function(issue, home = get_articles_path()) {
  issue_dir <- file.path(home, "Proofs", issue)
  issue_regex <- "^(\\d{4})-(\\d{1})"
  issue_year <- as.integer(sub(issue_regex, "\\1", issue))
  issue_vol <- issue_year - 2008L
  issue_num <- as.integer(sub(issue_regex, "\\2", issue))
  issue_month <- issue_month(issue)

  ## Handle articles
  issue_arts <- dir(issue_dir, pattern = "^\\d{4}-\\d{2,3}$", full.names = TRUE)
  arts <- lapply(file.path(issue_arts, "DESCRIPTION"), as.article)
  art_slugs <- vapply(arts, function(x) x$slug, character(1L))
  web_art_dir <- normalizePath(file.path(home, "..", "rjournal.github.io", "_articles"))
  art_online <- vapply(arts, has_status, logical(1L), "online")
  art_pub <- vapply(
    art_slugs,
    function(x) {
      if(identical(x, ""))
        FALSE
      else
        file.exists(file.path(web_art_dir, x, xfun::with_ext(x, "html")))
    }, logical(1L)
  )
  if (any(!art_pub)) {
    cli::cli_alert_warning("Some proofed articles have not yet been published, would you like to publish them now?")
    cli::cli_li(basename(issue_arts[!art_pub]))
    if (utils::menu(c("yes", "no")) == 1L) {
      lapply(
        issue_arts[!art_pub], publish_article,
        volume = issue_vol, issue = issue_num, home = home
      )
    }
  }

  ## Handle news
  issue_news <- dir(file.path(issue_dir, "news"), full.names = TRUE)
  web_news_dir <- normalizePath(file.path(home, "..", "rjournal.github.io", "_news"))
  news_slugs <- paste0("RJ-", issue, "-", basename(issue_news))
  news_pub <- file.exists(file.path(web_news_dir, news_slugs, xfun::with_ext(news_slugs, ".html")))
  if (any(!news_pub)) {
    cli::cli_alert_warning("Some news articles have not yet been published, would you like to publish them now?")
    cli::cli_li(basename(news_slugs[!news_pub]))
    if (utils::menu(c("yes", "no")) == 1L) {
      lapply(
        issue_news[!news_pub],
        publish_news,
        volume = issue_vol, issue = issue_num
      )
    }
  }

  ## Handle issue
  web_issue_dir <- normalizePath(file.path(home, "..", "rjournal.github.io", "_issues"))
  latest_web_issue <- max(dir(web_issue_dir, pattern = "^\\d{4}-\\d$"))
  xfun::dir_create(file.path(web_issue_dir, issue))
  issue_rmd <- file.path(web_issue_dir, issue, c(xfun::with_ext(issue, ".Rmd")))
  file.copy(
    file.path(web_issue_dir, latest_web_issue, c(xfun::with_ext(latest_web_issue, ".Rmd"), "Rlogo-5.png", "RJournal.sty")),
    file.path(web_issue_dir, issue, c(xfun::with_ext(issue, ".Rmd"), "Rlogo-5.png", "RJournal.sty"))
  )
  issue_yml <- partition_rmd(issue_rmd)$front_matter
  issue_yml$title <- paste0("Volume ", issue_vol, "/", issue_num)
  issue_yml$description <- paste("Articles published in the", issue_month, issue_year, "issue")
  issue_yml$date <- format(as.Date(paste(issue_year, issue_month, "01"), format = "%Y %B %d"))
  issue_yml$articles <- list(before = NULL, after = NULL)
  issue_yml$volume <- issue_vol
  issue_yml$issue <- issue_num
  issue_yml$news <- c(setdiff(basename(issue_news), "editorial"))
  update_front_matter(issue_yml, issue_rmd)

  edit_file <- utils::file.edit
  if(requireNamespace("rstudioapi")) {
    if(rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
      edit_file <- rstudioapi::navigateToFile
    }
  }
  edit_file(issue_rmd)
}

move_article_web <- function(from, to, volume, issue) {
  xfun::dir_create(to)
  slug <- basename(to)

  # Copy supporting files
  article_files <- setdiff(
    list.files(from),
    c("correspondence", "history", "RJournal.sty", "DESCRIPTION",
      "RJwrapper.pdf", "supplementaries.zip")
  )
  file.copy(
    file.path(from, article_files),
    to,
    overwrite = TRUE,
    recursive = TRUE
  )

  # obtain metadata from wrapper
  if(!file.exists(file.path(from, "RJwrapper.tex"))) {
    cli::cli_abort("Could not find RJwrapper.tex for {basename(from)}.")
  }
  wrapper_metadata <- readLines(file.path(from, "RJwrapper.tex"))
  ## construct preamble.tex for additional tex packages and macros

  ## obtain metadata
  pandoc_markdown <- function(article_dir) {
    # Copy modified style file for extracting metadata
    file.copy(
      system.file("pandoc-metadata/RJournal.sty", package = "rj"),
      file.path(article_dir, "RJournal.sty"),
      overwrite = TRUE
    )
    on.exit(
      file.remove(file.path(article_dir, "RJournal.sty"))
    )
    rmarkdown::pandoc_convert(
      "RJwrapper.tex", "markdown+raw_tex", output = metadata <- tempfile(fileext = ".md"),
      options = c("--standalone"), wd = paste0(article_dir,"/")
    )
    metadata
  }
  pandoc_metadata <- function(markdown){
    metadata <- rmarkdown::yaml_front_matter(markdown)

    # Separate address into fields
    metadata$author <- lapply(
      strsplit(metadata$address, "\\\n", fixed = TRUE),
      function(person) {
        author <- list(
          name = person[1],
          affiliation = person[2]
        )
        if(any(orcid <- grepl("^ORCiD:", person))) {
          author$orcid <- sub("^ORCiD: ", "", person[orcid])
        }
        if(any(email <- grepl("^email:", person))) {
          author$email <- sub("^email:", "", person[email])
        }
        fields <- logical(length(person))
        fields[1:2] <- TRUE
        if(any(address <- !(fields | orcid | email))) {
          author$address <- person[address]
        }
        author
      }
    )
    metadata$address <- NULL
    metadata
  }
  # collect metadata

  article_metadata <- if(file.exists(file.path(from, "DESCRIPTION"))) {
    art <- load_article(file.path(from, "DESCRIPTION"))
    online_date <- Filter(function(x) x$status == "online", art$status)
    online_date <- if (length(online_date) == 0) {
      Sys.Date()
    } else {
      online_date[[1]]$date
    }

    first_acknowledged <- Filter(function(x) x$status == "acknowledged", art$status)
    if(length(first_acknowledged) == 0) first_acknowledged <- list(art$status[[1]])

    list(
      slug = slug,
      acknowledged = first_acknowledged[[1]]$date,
      online = online_date
    )

  } else {
    # Produce minimal article metadata for news
    issue_year <- volume + 2008
    issue_month <- if(issue_year < 2022) issue * 6 else issue * 3
    list(
      slug = basename(to),
      online = as.Date(paste(volume + 2008, issue_month, "01"), format = "%Y %m %d")
    )
  }

  # Handle Rmd source files directly
  rmd_file <- list.files(from, pattern = "\\.(R|r)md$", full.names = TRUE)
  rmd_file <- Filter(
    function(x) {
      rmd_output <- partition_rmd(x)$front_matter$output
      if(is.list(rmd_output)) rmd_output <- names(rmd_output)
      "rjtools::rjournal_web_article" %in% rmd_output
    },
    rmd_file
  )
  if(length(rmd_file) == 1L) {
    rmd_yml <- partition_rmd(rmd_file)$front_matter
    if(is.null(rmd_yml$abstract)) cli::cli_abort("The abstract for {article_metadata$slug} is missing!")
    rmd_yml$date <- format_non_null(article_metadata$online)
    rmd_yml$date_received <- format_non_null(article_metadata$acknowledged)
    rmd_yml$volume <- volume
    rmd_yml$issue <- issue
    rmd_yml$slug <- article_metadata$slug
    file.copy(
      rmd_file,
      rmd_path <- file.path(to, xfun::with_ext(basename(to), ".Rmd")),
      overwrite = TRUE
    )
    update_front_matter(rmd_yml, rmd_path)
    if(requireNamespace("renv", quietly = TRUE)) {
      needed_packages <- setdiff(renv::dependencies(rmd_path)$Package, rownames(installed.packages()))
      if(length(needed_packages) > 0) {
        cli::cli_alert_warning("Article {article_metadata$slug} requires additional packages, would you like to install:")
        cli::cli_li(needed_packages)
        if (utils::menu(c("yes", "no")) == 1L) {
          install.packages(needed_packages)
        }
      }
    }
    return(rmd_path)
  }

  pandoc_markdown <- pandoc_markdown(from)
  pandoc_metadata <- pandoc_metadata(pandoc_markdown)
  # Find extra dependencies
  wrapper <- readLines(file.path(from, "RJwrapper.tex"))
  doc_start <- which(grepl("^\\s*\\\\begin\\{document\\}", wrapper))
  common_deps <- c(
    "\\documentclass[a4paper]{report}", "\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usepackage{RJournal}", "\\usepackage{amsmath,amssymb,array}",
    "\\usepackage{booktabs}", "", "%% load any required packages FOLLOWING this line"
  )
  extra_deps <- setdiff(wrapper[seq_len(doc_start)-1], common_deps)
  pdf_args <- list()
  if(length(extra_deps) > 0) {
    writeLines(extra_deps, file.path(to, "preamble.tex"))
    pdf_args$includes <- list(in_header = "preamble.tex")
  }

  # Make yaml front matter
  front_matter <- list(
    title = pandoc_metadata$title,
    abstract = pandoc_metadata$subject %||% paste0('The "', pandoc_metadata$title, '" article from the ', issue_year, '-', issue, ' issue.'),
    author = pandoc_metadata$author,
    date = format_non_null(article_metadata$online),
    date_received = format_non_null(article_metadata$acknowledged),
    journal = list(
      firstpage = article_metadata$pages[1],
      lastpage = article_metadata$pages[2]
    ),
    volume = as.integer(volume),
    issue = as.integer(issue),
    slug = article_metadata$slug,
    packages = list(
      cran = article_metadata$CRANpkgs,
      bioc = article_metadata$BIOpkgs
    ),
    preview = 'preview.png',
    bibliography = pandoc_metadata$bibliography,
    CTV = article_metadata$CTV_rev,
    output = list(
      `rjtools::rjournal_web_article` = list(
        self_contained = FALSE,
        toc = FALSE,
        legacy_pdf = TRUE
      )
      # `rjtools::rjournal_pdf_article` = pdf_args
    )
  )

  # Get article body
  pandoc_md_contents <- readLines(pandoc_markdown)
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", pandoc_md_contents)
  article_body <- c()
  if (delimiters[1] > 1)
    article_body <- c(article_body, pandoc_md_contents[1:delimiters[1] - 1])
  if (delimiters[2] < length(pandoc_md_contents))
    article_body <- c(article_body, pandoc_md_contents[-(1:delimiters[2])])

  # Write Rmd
  xfun::write_utf8(
    c("---", yaml::as.yaml(front_matter), "---", article_body),
    rmd_path <- file.path(to, xfun::with_ext(article_metadata$slug, ".Rmd"))
  )

  rmd_path
}

make_landing <- function(article_metadata, issue) {
  slug <- article_metadata$slug
  res <- paste0("---\nlayout: landing\nissue: ", issue, "\nslug: ", slug, "\ntitle: \"", str_sub(article_metadata$title, 1, 25), "...\"\n---\n\n")
  res
}


get_refs_from_tex <- function(article_path, final = FALSE) {
  RJw <- readLines(paste0(article_path, "/RJwrapper.tex"))
  str_search_inp <- "^\\s*((\\\\input\\{)([-a-zA-Z0-9_\\+\\.]*)(\\}))"
  inp_str <- c(na.omit(str_trim(str_extract(RJw, str_search_inp))))
  inps <- c(str_locate(inp_str, "((\\{)([-a-zA-Z0-9_\\+\\.]*)(\\}))"))
  inp_tex <- str_sub(inp_str, inps[1] + 1, inps[2] - 1)
  if (str_sub(inp_tex, str_length(inp_tex) - 3, str_length(inp_tex)) != ".tex") {
    inp_tex <- paste0(inp_tex, ".tex")
  }
  tex <- readLines(paste0(article_path, "/", inp_tex))
  Cp_str <- "((\\\\CRANpkg\\{)([a-zA-Z0-9\\.]*)(\\}))"
  Cps <- str_locate_all(tex, Cp_str)
  Cpsi <- sapply(Cps, nrow)
  CRANpkgs <- NULL
  if (any(Cpsi > 0)) {
    wCpsi <- which(Cpsi > 0)
    Cmat <- cbind(
      i = rep(wCpsi, sapply(Cps[wCpsi], nrow)),
      do.call("rbind", Cps[wCpsi])
    )
    CRANpkgs <- unique(apply(Cmat, 1, function(row) {
      str_sub(tex[row[1]], row[2] + 9, row[3] - 1)
    }))
  }
  Bp_str <- "((\\\\BIOpkg\\{)([a-zA-Z0-9\\.]*)(\\}))"
  Bps <- str_locate_all(tex, Bp_str)
  Bpsi <- sapply(Bps, nrow)
  BIOpkgs <- NULL
  if (any(Bpsi > 0)) {
    wBpsi <- which(Bpsi > 0)
    Bmat <- cbind(
      i = rep(wBpsi, sapply(Bps[wBpsi], nrow)),
      do.call("rbind", Bps[wBpsi])
    )
    BIOpkgs <- unique(apply(Bmat, 1, function(row) {
      str_sub(tex[row[1]], row[2] + 8, row[3] - 1)
    }))
  }
  ctv_str <- "((\\\\ctv\\{)([a-zA-Z0-9]*)(\\}))"
  ctvs <- str_locate_all(tex, ctv_str)
  ctvsi <- sapply(ctvs, nrow)
  CTVs <- NULL
  if (any(ctvsi > 0)) {
    wctvsi <- which(ctvsi > 0)
    CTVmat <- cbind(
      i = rep(wctvsi, sapply(ctvs[wctvsi], nrow)),
      do.call("rbind", ctvs[wctvsi])
    )
    CTVs <- unique(apply(CTVmat, 1, function(row) {
      str_sub(tex[row[1]], row[2] + 5, row[3] - 1)
    }))
  }
  res <- list(CRANpkgs = CRANpkgs, BIOpkgs = BIOpkgs, CTVs = CTVs)
  if (final) {
    str_search_start <- "((\\\\setcounter\\{page\\}\\{)([0-9]*)(\\}))"
    start_str <- c(na.omit(str_trim(str_extract(RJw, str_search_start))))
    start0 <- c(str_locate(start_str, "((\\{)([0-9]*)(\\}))"))
    start <- as.integer(str_sub(start_str, start0[1] + 1, start0[2] - 1))
    attr(res, "start") <- start
    cat("start", start, "\n")
  }
  res
}

#' Get metadata from a PDF file
#'
#' @param from pdf file
#' @inheritParams online_metadata_for_article
#'
#' @export
get_md_from_pdf <- function(from, final = FALSE) {
  toc <- pdftools::pdf_toc(from)
  title <- toc$children[[1L]]$title
  bibtitle <- str_wrap(title, width = 60, exdent = 10)
  text <- pdftools::pdf_text(from)

  t1s <- str_split(text[1], "\\n")[[1L]]
  abs_ <- which(!is.na(str_locate(t1s, "^[ ]*Abstract")[, "start"]))
  cat("abs_", abs_, "\n")
  aut0 <- paste(t1s[which(!is.na(str_locate(t1s, "^[ ]*by")[, "start"]))[1]:(abs_ - 1L)], collapse = " ")
  aut1 <- str_trim(str_replace(aut0, "by", ""))
  aut2 <- str_replace_all(aut1, ", and ", ", ")
  aut3 <- str_replace_all(aut2, " and ", ", ")
  author <- unlist(str_split(aut3, ", "))
  bibauthor <- str_wrap(paste(author, collapse = " and "), width = 60, exdent = 10)
  if (length(toc$children[[1L]]$children) >= 1L) {
    first_section <- which(!is.na(str_locate(t1s, paste0(
      "^[ ]*",
      str_sub(toc$children[[1L]]$children[[1L]]$title, 1L, 20L)
    ))[, "start"]))
  } else {
    first_section <- abs_ + 4L
  }
  abs0 <- paste(t1s[abs_:(first_section - 1L)], collapse = " ")
  abstract <- str_replace_all(substring(abs0, 10, nchar(abs0)), "[-][ ]", "")
  res <- list(
    author = author, title = title, abstract = abstract,
    bibtitle = bibtitle, bibauthor = bibauthor
  )
  cat(pdftools::pdf_info(from)$pages, " pages and final is", final, "\n")
  if (final) attr(res, "len") <- pdftools::pdf_info(from)$pages
  res
}

#' Build article from LaTeX
#' @inheritParams publish
#' @param share_path ???
#' @param clean Remove generated files
#' @export
build_latex <- function(article,
                        share_path = normalizePath("../share", mustWork = TRUE),
                        clean = TRUE) {
  article <- as.article(article)
  stopifnot(file.exists(share_path))

  # Check RJournal.sty does not exist
  # sty_path <- file.path(article$path, "RJournal.sty")
  # if (file.exists(sty_path)) {
  #  stop("Article contains RJournal style file", call. = FALSE)
  # }

  if(!file.exists(file.path(article$path, "RJwrapper.tex"))) {
    stop(sprintf(
      "Could not find RJwrapper.tex for %s", article$path
    ))
  }

  # Build latex
  in_dir(
    article$path,
    tinytex::pdflatex("RJwrapper.tex", texinputs = share_path, clean = clean)
  )
}

#' Generate metadata needed for website.
#'
#' @importFrom yaml as.yaml
#' @export
online_metadata <- function() {
  articles <- accepted_articles()
  articles <- Filter(function(x) !empty(x$slug), articles)
  lapply(articles, online_metadata_for_article)
}

#' Generate metadata for one article.
#'
#' @param x An article
#' @param final If in a final state, then also include page information.
#'
#' @export
online_metadata_for_article <- function(x, final = FALSE) {
  #    names <- vapply(x$authors, function(x) {
  #                        format(as.person(x),
  #                               include = c("given", "family"))[[1]]
  #                    }, FUN.VALUE = character(1L))
  from <- file.path(x$path, "RJwrapper.pdf")
  pdf_list <- get_md_from_pdf(from, final = final)
  refs_list <- get_refs_from_tex(x$path, final = final)
  if (final) {
    start <- attr(refs_list, "start")
    len <- attr(pdf_list, "len")
    pages <- as.integer(c(start, start + (len - 1)))
    cat("pages ", pages, start, len, "\n")
  }
  if (!is.null(refs_list$CRANpkgs)) {
    refs_list$CTV_rev <- rev_dep_ctv(refs_list$CRANpkgs)
  }
  landing <- str_sub(x$slug, 1L, 5L) == "RJ-20"
  sl <- as.data.frame(x$status)
  res <- c(list(
    slug = x$slug,
    title = pdf_list$title,
    bibtitle = pdf_list$bibtitle,
    author = pdf_list$author,
    bibauthor = pdf_list$bibauthor,
    abstract = pdf_list$abstract,
    acknowledged = format(sl$date[which(sl$status == "acknowledged")]),
    online = format(sl$date[min(which(sl$status == "online"))])
  ), refs_list[!sapply(refs_list, is.null)])

  if (!empty(x$suppl)) {
    zipfrom <- file.path(x$path, "supplementaries.zip")
    sz <- "unknown"
    if (file.exists(zipfrom)) {
      sz <- format(structure(file.size(zipfrom),
                             class = "object_size"
      ), standard = "IEC", units = "auto")
    }
    res <- c(res, list(suppl = sz))
  }

  if (landing) res <- c(res, list(landing = str_sub(x$slug, 4L, 7L)))
  if (final) res <- c(res, list(pages = pages))
  res
}

#' @method print catout
#' @export
print.catout <- function(x, ...) cat(x, "\n", sep = "")

make_slug <- function(names) {
  people <- as.person(names)
  family <- unlist(lapply(people, function(x) x[[1]]$family))
  if (length(family) > 3) {
    family <- c(family[1:3], "et-al")
  }

  family <- iconv(family, to = "ASCII//translit")
  family <- gsub("[^A-Za-z]", "", family)
  tolower(paste0(family, collapse = "-"))
}

bundle <- function(article, dest_path) {
  article <- as.article(article)
  dest <- file.path(dest_path, paste0(format(article$id), ".zip"))

  # Check confidential files are present, but don't include in zip file
  files <- dir(article$path)
  conf <- c("DESCRIPTION", "correspondence")

  if (length(intersect(files, conf)) != 2) {
    stop(paste(setdiff(conf, files), collapse = ", "), "not found in ", article$path)
  }
  files <- setdiff(files, conf)

  # Create zip file
  in_dir(article$path, zip(dest, files))
}

# library(yaml)
# config <- yaml.load_file("_config.yml")
# auths_accept <- unlist(lapply(config$issues[[1]]$articles, "[[", "author"))
# library(genderizeR)
# givenNames = findGivenNames(auths_accept)
# res <- genderize(auths_accept, genderDB = givenNames)
# table(res$gender)
# auths_accept <- lapply(config$issues, function(x) unlist(lapply(x$articles, "[[", "author")))
# names(auths_accept) <- sapply(config$issues, function(x) x$issue)
# tab_all <- lapply(auths_accept, function(x) {givenNames <- findGivenNames(x); genderize(x, genderDB = givenNames)})
# do.call("rbind", sapply(tab_all, function(x) table(x$gender)))
