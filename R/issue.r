
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
# made.

issue_year <- function(id) {
    as.integer(sub("-.*", "", id))
}

issue_number <- function(id) {
    as.integer(sub(".*-", "", id))
}

read_tex <- function(x) {
    structure(readLines(x), class="tex")
}

cmd_regex <- function(x) {
    paste0("^[[:space:]]*\\\\", x)
}

`$.tex` <- function(x, name) {
    type.convert(sub(".*\\{(.*?)\\}", "\\1",
                     grep(cmd_regex(name), x, value=TRUE)))
}

`$<-.tex` <- function(x, name, value) {
    line <- grep(cmd_regex(name), x)
    x[line] <- sub("\\{.*?\\}", paste0("{", value, "}"), x[line])
    x
}

previous_id <- function(id) {
    number <- issue_number(id) - 1L
    year <- issue_year(id)
    if (number == 0L) {
        year <- year - 1L
        number <- 1L
    }
    paste0(year, "-", number)
}

issue_dir <- function(id) {
    file.path("Proofs", id)
}

issue_file <- function(id) {
    file.path(issue_dir(id), paste0("RJ-", id, ".tex"))
}

# see notes, top of this file
make_proof <- function(id, share_path = file.path("..", "share"), exec=FALSE) {
    dir <- issue_dir(id)
    if (!file.exists(dir))
#        stop("Proof ", id, " already exists")
    dir.create(dir)

    prev_id <- previous_id(id)
    prev_dir <- file.path("Proofs", prev_id)
    inherited_files <- c("Rdsub.tex", "Rlogo-5.png")
    file.copy(file.path(prev_dir, inherited_files), dir)
    file.copy(file.path(share_path, "RJournal.sty"), dir)

    issue_file <- issue_file(id)
    file.copy(file.path(share_path, "RJournal_template.tex"), issue_file)
    issue <- read_tex(issue_file)

    number <- issue_number(id)
    if (number == 1L) {
        issue$volume <- issue$volume + 1L
    }
    issue$volnum <- number
    issue$year <- issue_year(id)
    issue$month <- switch(number, "1" = "June", "2" = "December")

    arts <- accepted_articles()
    ready <- filter_status(arts, "online")
    for (art in ready) {
        if (exec) {
          system(paste("git mv", art$path, file.path(dir, format(art$id))))
        } else {
          cat(art$path, file.path(dir, format(art$id)), "\n")
        }
    }

    message("Do not forget to update the editorial board in ", issue_file)
}

insert_replace <- function(tex, at, lines) {
    c(tex[1:(at - 1)], lines, tex[(at + 1):length(tex)])
}

insert_replace_n <- function(tex, at, lines, n) {
    c(tex[1:(at - 1)], lines, tex[(at + n):length(tex)])
}

#run in Proofs/issue-id
update_RJwrapper_page_no <- function(art_id, start, vol, no, yr, mon) {
  tex <- read_tex(file.path(art_id, "RJwrapper.tex"))
  pos <- grepl(cmd_regex("sectionhead"), tex)
  if (sum(pos) > 1L) {
    stop("Multiple \\sectionhead in ", file.path(art_id, "RJwrapper.tex"))
  }
  if (!any(pos)) {
    stop("No \\sectionhead found in ", file.path(art_id, "RJwrapper.tex"))
  }
  writeLines(tex, file.path(art_id, "orig_RJwrapper.tex"))
  n <- 5
  insert <- c("\\sectionhead{Contributed research article}",
    paste0("\\volume{", vol, "}"),
    paste0("\\volnumber{", no, "}"),
    paste0("\\year{", yr, "}"),
    paste0("\\month{", mon, "}"),
    paste0("\\setcounter{page}{", start, "}"))
#cat(which(pos), n, "\n")
  out <- insert_replace_n(tex, which(pos), insert, n)
  writeLines(out, file.path(art_id, "RJwrapper.tex"))
  insert[6]
}

#run in Proofs/issue-id
copy_RJwrapper_pdf_slug_archive <- function(art_id, arch_path, arch_yr) {
  slug <- as.article(art_id)$slug
  if (unlist(strsplit(slug, "-"))[2] != arch_yr)
    stop("wrong slug year:", unlist(strsplit(slug, "-"))[2])
  src <- file.path(art_id, "RJwrapper.pdf")
  dest <- file.path(arch_path, arch_yr, slug, paste0(slug, ".pdf"))
  file.copy(src, dest, overwrite=TRUE)
  dest
}

convert_bbl_tex <- function(tex_path) {
    tex <- read_tex(tex_path)
    has_bib <- grepl(cmd_regex("bibliography"), tex)
    if (sum(has_bib) > 1L) {
        stop("Multiple \\bibliography commands in ", tex_path)
    }
    if (!any(has_bib)) {
        message("No \\bibliography found. Skipping ", tex_path)
        return(tex_path)
    }

    bib <- tex$bibliography
    if (tools::file_ext(bib) != "bib")
        bib <- paste0(bib, ".bib")
    bib_path <- file.path(dirname(tex_path), bib)
    if (!file.exists(bib_path)) stop("Can't find ", bib_path)

    ## Build tex to make sure bib up-to-date message(share_path)
    message("Building ", tex_path)
    owd <- getwd()
    setwd(dirname(tex_path))
    tools::texi2pdf("RJwrapper.tex", clean=FALSE, texinput=owd)
    setwd(owd)
    bbl_path <- file.path(dirname(tex_path), "RJwrapper.bbl")
    if (!file.exists(bbl_path)) stop("Can't find ", bbl_path)

    ## Read bbl and use to replace bib
    bbl <- readLines(bbl_path)
    bib_line <- which(has_bib)
    out <- insert_replace(tex, bib_line, bbl)
    # change by NM, Sept. 1, 2019; bbl path wrong
    # writeLines(out, sub("/", "/bbl-", tex_path))
    slashes <- str_locate_all(tex_path, '/')[[1]]
    sl3 <- slashes[3,1]  # third slash loc
    bbltex_path <- 
       paste0(substr(tex_path,1,sl3-1),
              '/bbl-',
              substr(tex_path,sl3+1,nchar(tex_path)))
    writeLines(out,bbltex_path)

    bbltex_path
}

convert_one_bbl <- function(id) {
    path <- as.article(id)$path
    wrapper_tex <- read_tex(file.path(path, "RJwrapper.tex"))
    tex_input <- wrapper_tex$input
    tex_path <- file.path(path, tex_input)
    if (length(tex_path) > 1L) {
        stop("Multiple tex files included: ", paste(tex_path, collapse=", "))
    }
    if (tools::file_ext(tex_path) != "tex")
        tex_path <- paste0(tex_path, ".tex")

    convert_bbl_tex(tex_path)
}

convert_bbl <- function(articles) {
    unlist(lapply(articles, convert_one_bbl))
}

article_paths <- function(id) {
    paths <- dir(file.path("Proofs", id), pattern="[0-9]{4}-[0-9]+$",
                 full.names=TRUE)
    paths[file.info(paths)[,"isdir"]]
}

convert_issue_bbl <- function(id) {
    art_paths <- article_paths(id)
    convert_bbl(art_paths)
}

build_include <- function(tex) {
    paste0(
        "\\begin{article}\n",
        "\\subimport{", basename(dirname(tex)), "/}{", basename(sub("/", "/bbl-", tex)), "}\n",
        "\\end{article}\n",
        "\\newpage\n")
}

# see notes, top of this file
#' Build an issue
#'
#' Embeds the article bibliographies as bbl, adds the article import
#' stanzas to the template, and compiles the proof as PDF.
#'
#' @param id the id of the issue
#' @export
build_issue <- function(id) {
    files <- convert_issue_bbl(id)
#     notproofed <- checkProofed(files)
#     if (notproofed != '') {
#        print('these articles not yet proofed, not processed')
#        print(notproofed)
#        files <- setdiff(files,notproofed)
#     }

    issue_file <- issue_file(id)
    issue_lines <- readLines(issue_file)

### TODO: also include tex files under '/news'
    include_lines <- unlist(lapply(files, build_include))
    # NM, Sept. 3, 2019: 'pos' was nonexistent, fixed now
    pos <- grep("Contributed Research Articles", issue_lines)
    if (length(pos) > 0L) {
        out <- insert_replace(issue_lines, pos, include_lines)
        writeLines(out, issue_file)
    }

    in_dir(dirname(issue_file), system(paste("pdflatex", basename(issue_file))))
}

# checkProofed <- function(files) {
#    grepProofed <- function(fl) {
#       bblloc <- str_locate(fl,'bbl-')
#       desfl <- paste0(substr(fl,1,(bblloc-1)),'DESCRIPTION')
#       cmd <- paste0('grep proofed ',desfl)
#       sysout <- system(cmd,intern=T)
#       length(sysout) == 0
#    }
#    notThere <- Map(grepProofed,files)
#    notThere <- unlist(notThere)
#    names(which(notThere))
# }

## run from articles
init_archive_path <- function(id, web_path) {
    archive_path <- file.path(web_path, "archive", id)
    if (file.exists(archive_path))
        unlink(archive_path, recursive=TRUE)
    dir.create(archive_path)

    issue_file <- issue_file(id)
    issue <- read_tex(issue_file)

    write_header <- function(header, file) {
        writeLines(c("---", as.yaml(header), "---"), file)
    }

    header <- list(layout = "issue_landing-new",
                   title = paste0("Volume ", issue$volume, "/", issue$volnum,
                                  ", ", issue$month, " ", issue$year),
                   issue = id)
    write_header(header, file.path(archive_path, "index.html"))

    header_bib <- header
    header_bib$layout <- "issue-bib-new"
    write_header(header_bib, file.path(archive_path, "index-bib.html"))

    file.copy(replace_ext(issue_file(id), "pdf"), archive_path)

    archive_path
}

## run from articles
cleanup_accepted <- function(id, web_path) {
    slugs <- lapply(article_paths(id), function(a) as.article(a)$slug)

print(slugs)

#    obsolete_pdfs <- file.path(web_path, "archive", "accepted",
#                               paste0(slugs, ".pdf"))
#    unlink(obsolete_pdfs)

    config_path <- file.path(web_path, "_config.yml")
    config <- yaml.load_file(config_path)
    writeLines(as.yaml(config), file.path(web_path, "safe_config.safe"))
    config_arts <- config$issues[[1L]]$articles
    config_slugs <- vapply(config_arts, `[[`, character(1L), "slug")
    config$issues[[1L]]$articles <- config_arts[!config_slugs %in% slugs]
    writeLines(as.yaml(config), config_path)
}

pre_metadata <- function(id) {
    tex <- read_tex(issue_file(id))
    eic <- trimws(tex[grep("Editor-in-Chief", tex) + 1L])
    list(list(title = "Editorial", author = eic, slug = "editorial"))
}

article_metadata <- function(p) {
    art <- as.article(p)
    art$author <- vapply(art$authors, "[[", character(1L), "name")
    if (is.null(art$slug) || art$slug == "") {
        art$slug <- make_slug(art$author)
    }
    art[c("title", "author", "slug")]
}

articles_metadata <- function(id) {
    lapply(article_paths(id), article_metadata)
}

issue_news_metadata <- function(news) {
    tex <- read_tex(news)
    list(title = tex$title, author = tex$author,
         slug = tools::file_path_sans_ext(basename(news)))
}

post_lp_metadata <- function(id, fs=c("foundation", "cran", "bioc", "ch")) {
    files <- paste0(c("foundation", "cran", "bioc", "ch"), ".tex")
    news <- file.path(issue_dir(id), "news", files)
    lapply(news[file.exists(news)], issue_news_metadata)
}

bibtex_escape_case <- function(x) {
    ## Tries to escape the names of languages and packages
    sub(" ([[:lower:].]+)$", " {\\1}",
        sub("^([[:lower:].]+)", "{\\1}",
            gsub(" ([[:upper:]]+[[:upper:][:digit:].]*[+]*)($|[:, ])",
                 " {\\1}\\2",
                 gsub("(\\w+[[:upper:].]+[[:alnum:]*.]*)", "{\\1}",
                      x))))
}

bibtex_encode_non_ascii <- function(x) {
    map <- c("á" = "{\\\' a}",
             "ä" = "{\\\" a}",
             "é" = "{\\\' e}",
             "í" = "{\\\' i}",
             "ï" = "{\\\" i}",
             "ñ" = "{\\~ n}",
             "ó" = "{\\\' o}")
    Encoding(names(map)) <- "UTF-8"
    for (i in seq_along(map)) {
        x <- gsub(names(map)[i], map[i], x, fixed=TRUE)
    }
    x
}

annotate_metadata <- function(article, start, end) {
    bibtitle <- bibtex_escape_case(article$title)
    if (bibtitle != article$title)
        article$bibtitle <- bibtitle
    bibauthor <- bibtex_encode_non_ascii(article$author)
    if (!identical(bibauthor, article$author))
        article$bibauthor <- bibauthor
    article$pages <- as.integer(c(start, end))
    article$slug <- tolower(article$slug)
    article
}

contents_metadata <- function(id) {
    pre_md <- pre_metadata(id)
    art_md <- articles_metadata(id)
    post_md <- post_metadata(id)
    md <- c(pre_md, art_md, post_md)
    pb <- page_bounds(id)
    md <- mapply(annotate_metadata, md, pb$start, pb$end, SIMPLIFY=FALSE)
    md <- append(md, list(list(heading = "Contributed Research Articles")),
                 after = length(pre_md))
    append(md, list(list(heading = "News and Notes")),
           after = length(pre_md) + length(art_md) + 1L)
}

contents_lp_metadata <- function(id, post_file=c("foundation"=1, "erum"=3,
        "cran"=1, "bioc"=1, "ch"=1)) {
    setwd(file.path("Proofs", id))
    md <- list()
    pre_md <- get_pre_md()
    md <- c(md, list(pre_md))
    md <- c(md, list(list(heading = "Contributed Research Articles")))
    load("articles.RData")
    art_mds <- list()
    for (art in articles) {
        cat(art, "\n")
        art_mds[[art]] <- online_metadata_for_article(as.article(art),
            final=TRUE)
    }
    md <- c(md, art_mds)
    md <- c(md, list(list(heading = "News and Notes")))
    post_md <- get_post_md("news", file=post_file)
    md <- c(md, post_md)
    setwd("../..")
    md
}

issue_metadata <- function(id) {
    tex <- read_tex(issue_file(id))
    list(issue=id,
         year=tex$year,
         volume=tex$volume,
         num=tex$volnum,
         month=tex$month,
         bibmonth=tolower(tex$month),
         articles=contents_metadata(id))
}

issue_lp_metadata <- function(id, post_file=c("foundation"=1, "cran"=1, "bioc"=1, "ch"=1)) {
    tex <- read_tex(issue_file(id))
    list(issue=id,
         year=tex$year,
         volume=tex$volume,
         num=tex$volnum,
         month=tex$month,
         bibmonth=tolower(tex$month),
         articles=unname(contents_lp_metadata(id, post_file=post_file)))
}

write_issue_metadata <- function(web_path, md) {
    config_path <- file.path(web_path, "_config.yml")
    config <- yaml.load_file(config_path)
    issues <- config$issues
    ids <- vapply(issues, `[[`, character(1L), "issue")
    issues <- issues[ids != md$issue]
    config$issues <- c(issues, list(md))
    writeLines(as.yaml(config), config_path)
}

replace_ext <- function(f, new) {
    paste0(tools::file_path_sans_ext(f), ".", new)
}

page_bounds <- function(id) {
    toc_path <- replace_ext(issue_file(id), "toc")
    toc <- readLines(toc_path)
    chapters <- toc[grepl("{chapter}", toc, fixed=TRUE)]
    start <- as.integer(sub(".*\\}\\{([0-9]+)\\}\\{.*", "\\1", chapters))
    pdf_path <- replace_ext(toc_path, "pdf")
    pages <- pdftools::pdf_info(pdf_path)$pages
    end <- as.integer(c(start[-1L] - 1L, pages))
#    end[1L] <- start[1L] ## For editorial (don't count extra blank page)
    data.frame(start, end)
}

write_article_pdfs <- function(id, archives_path, md) {
    issue_pdf <- replace_ext(issue_file(id), "pdf")
    for(x in md) {
        if (!is.null(x$heading)) next
        pdftk(issue_pdf,
              "cat ", x$pages[1], "-", x$pages[2], " ",
              "output ", file.path(archives_path, paste0(x$slug, ".pdf")))
    }
}

pdftk <- function(input, ...) {
    input <- shQuote(input)
    args <- paste0(..., collapse = " ")

    cmd <- paste0("pdftk ", input, " ", args)
    message(cmd)
    res <- system(cmd, intern = TRUE)

    if (!is.null(attr(res, "status"))) {
        stop("Non-zero exit: ", attr(res, "status"), "\n", attr(res, "errmsg"),
             call. = FALSE)
    }
    res
}

update_layout <- function(id, web_path) {
    path <- file.path(web_path, "_layouts", "default.html")
    lines <- readLines(path)
    lines[which(lines == '- text: "Current Issue"') + 1L] <-
        paste0("  url: \"/archive/", id, "\"")
    writeLines(lines, path)
}

#' Publish an issue
#'
#' Generates per-article PDFs and copies them to the website, located
#' at \code{web_path}. Removes the published articles from the
#' accepted directory. Generates the necessary metadata and updates
#' the website configuration.
#'
#' This depends on the pdftools CRAN package, which in turn depends on
#' the poppler system library. It also requires the command line
#' program pdftk (distributed as PDFtk Server).
#'
#' @param id the id of the issue
#' @param web_path path to the rjournal.github.io checkout
#' @export
publish_issue <- function(id, web_path=file.path("..", "rjournal.github.io"), post_file=c("foundation"=1, "cran"=1, "bioc"=1, "ch"=1)) {
    if (!file.exists(web_path))
        stop("web_path '", web_path, "' does not exist, please specify")
    md <- issue_lp_metadata(id, post_file=post_file)
    archive_path <- init_archive_path(id, web_path)
    write_article_pdfs(id, archive_path, md$articles)
    cleanup_accepted(id, web_path)
    write_issue_metadata(web_path, md)
    update_layout(id, web_path)
}


get_startpg_from_nonart_tex <- function(dir, file) {
  RJw <- readLines(file.path(dir, paste0("wrapped_", file, ".tex")))
  str_search_start <- "((\\\\setcounter\\{page\\}\\{)([0-9]*)(\\}))"
  start_str <- c(na.omit(str_trim(str_extract(RJw, str_search_start))))
  start0 <- c(str_locate(start_str, "((\\{)([0-9]*)(\\}))"))
  start <- as.integer(str_sub(start_str, start0[1]+1, start0[2]-1))
  start
}

get_md_from_nonart_pdf <- function(from, nautlns=1) {
   from <- paste0(from, ".pdf")
   toc <- pdftools::pdf_toc(from)
   title <- toc$children[[1L]]$title
   bibtitle <- str_wrap(title, width=60, exdent=10)
   text <- pdftools::pdf_text(from)

   t1s <- str_split(text[1], "\\n")[[1L]]
   byln <- which(!is.na(str_locate(t1s, "^[ ]*by")[, "start"]))[1]
   aut0 <- paste(t1s[byln:(byln+nautlns-1)], collapse=" ")
   aut1 <- str_trim(str_replace(aut0, "by", ""))
   aut2 <- str_replace_all(aut1, ", and ", ", ")
   aut3 <- str_replace_all(aut2, " and ", ", ")
   author <- unlist(str_split(aut3, ", "))
   bibauthor <- str_wrap(paste(author, collapse=" and "), width=60, exdent=10)
   res <- list(author=author, title=title, bibtitle=bibtitle,
     bibauthor=bibauthor)
   attr(res, "len") <- pdftools::pdf_info(from)$pages
   res
}

get_pre_md <- function(dir="editorial", file="editorial") {
  res <- get_md_from_nonart_pdf(file.path(dir, file))
  start <- get_startpg_from_nonart_tex(dir, file)
  res$pages <- as.integer(c(start, start+attr(res, "len")-1))
  attr(res, "len") <- NULL
  res <- c(list(slug=file, cat="Editorial"), res)
  res
}

get_post_md <- function(dir="news", file=c("foundation"=1, "cran"=1, "bioc"=1, "ch"=1)) {
  res <- vector(mode="list", length=length(file))
  nms <- names(file)
  for (i in seq(along=file)) {
    resi <- get_md_from_nonart_pdf(file.path(dir, nms[i]), unname(file[i]))
    starti <- get_startpg_from_nonart_tex(dir, nms[i])
    resi$pages <- as.integer(c(starti, starti+attr(resi, "len")-1))
    attr(resi, "len") <- NULL
    res[[i]] <- c(list(slug=nms[i]), resi)
  }
  res
}


#init_archive_path(id, web_path)
#md <- issue_lp_metadata(id)
#write_article_pdfs(id, archive_path, md$articles) ??

#write_non_articles_pdf(???)

#cleanup_accepted(id, web_path)
#write_issue_metadata(web_path, md)
#update_lp_layout(id, web_path) ??



# run in Proofs/<id>
#arts0 <- list.files(patt="^201[0-9]-[0-9][0-9]*")
#arts <- strsplit(arts0, "-")
#arts1 <- t(sapply(arts, as.integer))
#arts2 <- arts0[order(arts1[,1], arts1[,2])]
#articles <- arts2
#save(articles, file="articles.RData")

#bib_keys <- list()
# for (i in seq(along=articles)) {
#bib_keys[[articles[i]]] <- rj:::find_stack_bib_keys(articles[i])

#for (ii in seq(along=bib_keys)) { if(ii > 1) { nms <- names(bib_keys); cat(ii, nms[1], ":", nms[ii-1], "|", nms[i]); cat(" ", any(unlist(bib_keys[1:(ii-1)]) %in% bib_keys[[ii]])); cat("\n") }}
#bib_keys[[i]][which(bib_keys[[i]] %in% unlist(bib_keys[1:(i-1)]))]
#}


# for (i in seq(along=articles)) {
#rj:::convert_one_bbl(articles[i])
#}

#run from articles
#pb_2017_1_contr <- rj:::page_bounds(<id>)[2:(length(articles)+1),]

#pb_2017_1_contr$what <- articles
#save(pb_2017_1_contr, file="Proofs/<id>/pb_2017_1_contr.RData")


#load("pb_2017_1_contr.RData")

#this_art <- 1L
#rj:::update_RJwrapper_page_no(as.character(pb_2017_1_contr$what[this_art]), as.character(pb_2017_1_contr$start[this_art]), "9", "1", "2017", "June")
#build_latex(as.character(pb_2017_1_contr$what[this_art]), "/home/rsb/proj/R-Journal/share")
#this_art <- this_art + 1L

#this_art <- 1L
#rj:::copy_RJwrapper_pdf_slug_archive(as.character(pb_2017_1_contr$what[this_art]), "../../../rjournal.github.io/archive", "2017")
#this_art <- this_art + 1L; pb_2017_1_contr$what[this_art]
#rj:::copy_RJwrapper_pdf_slug_archive(as.character(pb_2017_1_contr$what[this_art]), "../../../rjournal.github.io/archive", "2017")





#rj:::online_metadata_for_article(as.article("2015-17"), final=TRUE)
# run in Proofs/<id>
#load("articles.RData")
#art_mds <- list()
#for (art in articles) { cat(art, "\n"); art_mds[[art]] <- rj:::online_metadata_for_article(as.article(art), final=TRUE)}
