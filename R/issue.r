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

make_proof <- function(id, share_path = file.path("..", "share"), exec=FALSE) {
    dir <- issue_dir(id)
    if (!file.exists(dir))
#        stop("Proof ", id, " already exists")
    dir.create(dir)

    prev_id <- previous_id(id)
    prev_dir <- file.path("Proofs", prev_id)
    inherited_files <- c("Rdsub.tex", "Rlogo.png", "Rlogo-5.png",
                         "editorial.tex")
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
    writeLines(out, sub("/", "/bbl-", tex_path))
    
    sub("/", "/bbl-", tex_path)
}

convert_one_bbl <- function(id) {
    path <- rj::as.article(id)$path
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

#' Build an issue
#'
#' Embeds the article bibliographies as bbl, adds the article import
#' stanzas to the template, and compiles the proof as PDF.
#'
#' @param id the id of the issue
#' @export
build_issue <- function(id) {
    files <- convert_issue_bbl(id)

    issue_file <- issue_file(id)
    issue_lines <- readLines(issue_file)

### TODO: also include tex files under '/news'
    include_lines <- unlist(lapply(files, build_include))
    pos <- grep("^% include import stanzas here", issue_lines)
    if (length(pos) > 0L) {
        out <- insert_replace(issue_lines, pos, include_lines)
        writeLines(out, issue_file)
    }

    in_dir(dirname(issue_file), system(paste("pdflatex", basename(issue_file))))
}

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

post_metadata <- function(id) {
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
    end <- c(start[-1L] - 1L, pages)
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
publish_issue <- function(id, web_path=file.path("..", "rjournal.github.io")) {
    if (!file.exists(web_path))
        stop("web_path '", web_path, "' does not exist, please specify")
    md <- issue_metadata(id)
    archive_path <- init_archive_path(id, web_path)
    write_article_pdfs(id, archive_path, md$articles)
    cleanup_accepted(id, web_path)
    write_issue_metadata(web_path, md)
    update_layout(id, web_path)
}
