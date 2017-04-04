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
publish <- function(article, home = getwd(), legacy=FALSE) {
  article <- as.article(article)

  # Make sure we're in the right place
  if (basename(home) != "articles") {
    stop("Publish should be run from articles directory", call. = FALSE)
  }
  web_path <- normalizePath("../rjournal.github.io", mustWork = TRUE)
  share_path <- normalizePath("../share", mustWork = TRUE)
  
  if (!any(as.data.frame(article$status)$status == "accepted"))
    stop("not yet accepted")
  if (!any(as.data.frame(article$status)$status == "style checked"))
    stop("not yet style checked")

  message("Publishing ", format(article$id))
  # Build latex 
  build_latex(article, share_path)
  from <- file.path(article$path, "RJwrapper.pdf")

  if (legacy) {
    # create slug if needed and path to new home
    if (empty(article$slug) || str_sub(article$slug, 1L, 5L) == "RJ-20") {
      names <- unlist(lapply(article$authors, "[[", "name"))
      slug <- make_slug(names)
    } else {
      slug <- article$slug
    }
    to <- file.path(web_path, "archive", "accepted", paste0(slug, ".pdf"))

  } else {
    # stage new YYYY/RJ-YYYY-XXX landing directory and slug
    yr_id <- format(Sys.Date(), "%Y")
    if (!dir.exists(file.path(web_path, "archive", yr_id)))
      dir.create(file.path(web_path, "archive", yr_id))
    current_dirs <- list.files(file.path(web_path, "archive", yr_id))
    i <- 0L
    if (length(current_dirs) > 0) {
      if (any(nchar(current_dirs) != 11L)) stop("landing dir-name length error")
    } else {
      i <- 1L
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
      dir.create(file.path(web_path, "archive", yr_id, slug))
    } else{
      slug <- article$slug
    }
    landing_path <- file.path(web_path, "archive", yr_id, slug)
    to <- file.path(landing_path, paste0(slug, ".pdf"))
  }

  # copy PDF to target
  file.copy(from, to, overwrite = TRUE)
  message("Creating ", basename(to))

  article$slug <- slug
  article <- update_status(article, "online")

  # collect metadata
  article_metadata <- online_metadata_for_article(article)
  # if not legacy, create and post landing index.html
  if (!legacy) {
    issue <- "accepted"
    article_landing <- make_landing(article_metadata, issue)
    writeLines(article_landing, file.path(web_path, "archive", yr_id,
      slug, "index.html"))
  }

  ## Make yaml
  yaml_path <- file.path(web_path, "_config.yml")
  message("Updating ", yaml_path)
  config <- yaml.load_file(yaml_path)
  conf_articles <- config$issues[[1L]]$articles
  has_slug <- sapply(conf_articles, function(x) !is.null(x$slug))
  if (length(has_slug) > 0L) {
    has_slug_ptr <- character(length(has_slug))
    has_slug_ptr[has_slug] <- sapply(conf_articles[has_slug],
      function(x) x$slug)
    this_slug <- which(slug == has_slug_ptr)
    if (length(this_slug) == 0L) {
      conf_articles <- c(conf_articles, list(article_metadata))
    } else {
      conf_articles[[this_slug]] <- article_metadata
    }
  } else {
    conf_articles <- c(conf_articles, list(article_metadata))
  }
  config$issues[[1L]]$articles <- conf_articles
  writeLines(as.yaml(config), yaml_path)
  
  message("Remember to check changes into git")
  invisible(TRUE)
}


make_landing <- function(article_metadata, issue){
  slug <- article_metadata$slug
  res <- paste0("---\nlayout: landing\nissue: ", issue, "\nslug: ", slug, "\ntitle: \"", str_sub(article_metadata$title, 1, 25), "...\"\n---\n\n")
  res
}


get_refs_from_tex <- function(article_path) 
{
  RJw <- readLines(paste0(article_path, "/RJwrapper.tex"))
  str_search_inp <- "((\\\\input\\{)([-a-zA-Z0-9_\\+\\.]*)(\\}))"
  inp_str <- c(na.omit(str_trim(str_extract(RJw, str_search_inp))))
  inps <- c(str_locate(inp_str, "((\\{)([-a-zA-Z0-9_\\+\\.]*)(\\}))"))
  inp_tex <- str_sub(inp_str, inps[1]+1, inps[2]-1)
  if (str_sub(inp_tex, str_length(inp_tex)-3, str_length(inp_tex)) != ".tex")
    inp_tex <- paste0(inp_tex, ".tex")
  tex <- readLines(paste0(article_path, "/", inp_tex))
  Cp_str <- "((\\\\CRANpkg\\{)([a-zA-Z0-9\\.]*)(\\}))"
  Cps <- str_locate_all(tex, Cp_str)
  Cpsi <- sapply(Cps, nrow)
  CRANpkgs <- NULL
  if (any(Cpsi > 0)) {
    wCpsi <- which(Cpsi > 0)
    Cmat <- cbind(i=rep(wCpsi, sapply(Cps[wCpsi], nrow)),
      do.call("rbind", Cps[wCpsi]))
    CRANpkgs <- apply(Cmat, 1, function(row) {
      str_sub(tex[row[1]], row[2]+9, row[3]-1)
      })
  }
  Bp_str <- "((\\\\BIOpkg\\{)([a-zA-Z0-9\\.]*)(\\}))"
  Bps <- str_locate_all(tex, Bp_str)
  Bpsi <- sapply(Bps, nrow)
  BIOpkgs <- NULL
  if (any(Bpsi > 0)) {
    wBpsi <- which(Bpsi > 0)
    Bmat <- cbind(i=rep(wBpsi, sapply(Bps[wBpsi], nrow)),
      do.call("rbind", Bps[wBpsi]))
    BIOpkgs <- apply(Bmat, 1, function(row) {
      str_sub(tex[row[1]], row[2]+8, row[3]-1)
    })
  }
  ctv_str <- "((\\\\ctv\\{)([a-zA-Z0-9]*)(\\}))"
  ctvs <- str_locate_all(tex, ctv_str)
  ctvsi <- sapply(ctvs, nrow)
  CTVs <- NULL
  if (any(ctvsi > 0)) {
    wctvsi <- which(ctvsi > 0)
    CTVmat <- cbind(i=rep(wctvsi, sapply(ctvs[wctvsi], nrow)),
      do.call("rbind", ctvs[wctvsi]))
    CTVs <- apply(CTVmat, 1, function(row) {
      str_sub(tex[row[1]], row[2]+5, row[3]-1)
      })
  }
  list(CRANpkgs=CRANpkgs, BIOpkgs=BIOpkgs, CTVs=CTVs)
}

get_md_from_pdf <- function(from)
{
   toc <- pdftools::pdf_toc(from)
   title <- toc$children[[1L]]$title
   bibtitle <- str_wrap(title, width=60, exdent=10)
   text <- pdftools::pdf_text(from)
   
   t1s <- str_split(text[1], "\\n")[[1L]]
   abs_ <- which(!is.na(str_locate(t1s, "^[ ]*Abstract")[, "start"]))
   aut0 <- paste(t1s[which(!is.na(str_locate(t1s, "^[ ]*by")[, "start"]))[1]:(abs_-1L)], collapse=" ")
   aut1 <- str_trim(str_replace(aut0, "by", ""))
   aut2 <- str_replace_all(aut1, ", and ", ", ")
   aut3 <- str_replace_all(aut2, " and ", ", ")
   author <- unlist(str_split(aut3, ", "))
   bibauthor <- str_wrap(paste(author, collapse=" and "), width=60, exdent=10)
   if (length(toc$children[[1L]]$children) >= 1L) {
     first_section <- which(!is.na(str_locate(t1s, paste0("^[ ]*",
       str_sub(toc$children[[1L]]$children[[1L]]$title, 1L, 20L)))[, "start"]))
   } else {
     first_section <- abs_ + 4L
   }
   abs0 <- paste(t1s[abs_:(first_section-1L)], collapse=" ")
   abstract <- str_replace_all(substring(abs0, 10, nchar(abs0)), "[-][ ]", "")
   res <- list(author=author, title=title, abstract=abstract,
     bibtitle=bibtitle, bibauthor=bibauthor)
   res
}

#' Build article from LaTeX
#' 
#' @export
build_latex <- function(article,
                        share_path = normalizePath("../share", mustWork = TRUE),
                        clean = TRUE)
{
  article <- as.article(article)
  stopifnot(file.exists(share_path))
  
  # Check RJournal.sty does not exist
  sty_path <- file.path(article$path, "RJournal.sty")
  if (file.exists(sty_path)) {
    stop("Article contains RJournal style file", call. = FALSE)
  }

  # Build latex
  in_dir(article$path,
    texi2pdf("RJwrapper.tex", texinputs = share_path, clean = clean)
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

online_metadata_for_article <- function(x) {
#    names <- vapply(x$authors, function(x) {
#                        format(as.person(x),
#                               include = c("given", "family"))[[1]]
#                    }, FUN.VALUE = character(1L))
    from <- file.path(x$path, "RJwrapper.pdf")
    pdf_list <- get_md_from_pdf(from)
    refs_list <- get_refs_from_tex(x$path)
    if (!is.null(refs_list$CRANpkgs))
      refs_list$CTV_rev <- rev_dep_ctv(refs_list$CRANpkgs)
    landing <- str_sub(x$slug, 1L, 5L) == "RJ-20"
    sl <- as.data.frame(x$status)
    res <- c(list(
        title = pdf_list$title,
        bibtitle = pdf_list$bibtitle,
        slug = x$slug,
        author = pdf_list$author,
        bibauthor = pdf_list$bibauthor,
        abstract = pdf_list$abstract,
        acknowledged = format(sl$date[which(sl$status == "acknowledged")]),
        online = format(sl$date[max(which(sl$status == "online"))])
        ), refs_list[!sapply(refs_list, is.null)])
     if (landing) res <- c(res, list(landing = str_sub(x$slug, 4L, 7L)))
     res
}

#' @S3method print catout
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

#library(yaml)
#config <- yaml.load_file("_config.yml")
#auths_accept <- unlist(lapply(config$issues[[1]]$articles, "[[", "author"))
#library(genderizeR)
#givenNames = findGivenNames(auths_accept)
#res <- genderize(auths_accept, genderDB = givenNames)
#table(res$gender)
#auths_accept <- lapply(config$issues, function(x) unlist(lapply(x$articles, "[[", "author")))
#names(auths_accept) <- sapply(config$issues, function(x) x$issue)
#tab_all <- lapply(auths_accept, function(x) {givenNames <- findGivenNames(x); genderize(x, genderDB = givenNames)})
#do.call("rbind", sapply(tab_all, function(x) table(x$gender)))

