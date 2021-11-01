# find_stack_bib_keys <- function(article) {
#   article <- as.article(article)
#   article_path <- article$path
#   RJw <- readLines(paste0(article_path, "/RJwrapper.tex"))
#   str_search_inp <- "((\\\\input\\{)([-a-zA-Z0-9_\\+\\.]*)(\\}))"
#   inp_str <- c(na.omit(str_trim(str_extract(RJw, str_search_inp))))
#   inps <- c(str_locate(inp_str, "((\\{)([-a-zA-Z0-9_\\+\\.]*)(\\}))"))
#   inp_tex <- str_sub(inp_str, inps[1] + 1, inps[2] - 1)
#   if (str_sub(inp_tex, str_length(inp_tex) - 3, str_length(inp_tex)) != ".tex") {
#     inp_tex <- paste0(inp_tex, ".tex")
#   }
#   tex <- readLines(paste0(article_path, "/", inp_tex))
#   str_search_bib <- "((\\\\bibliography\\{)([-a-zA-Z0-9_\\+\\.]*(\\})))"
#   bib_str <- c(na.omit(str_trim(str_extract(tex, str_search_bib))))
#   bibs <- c(str_locate(bib_str, "((\\{)([-a-zA-Z0-9_\\+\\.]*)(\\}))"))
#   inp_bib <- str_sub(bib_str, bibs[1] + 1, bibs[2] - 1)
#   if (str_sub(inp_bib, str_length(inp_bib) - 3, str_length(inp_bib)) != ".bib") {
#     inp_bib <- paste0(inp_bib, ".bib")
#   }
#   tex <- readLines(paste0(article_path, "/", inp_tex))
#   bibfile <- paste0(article_path, "/", inp_bib)
#   if (!file.exists(bibfile)) stop(bibfile, " not found in ", article_path)
#   # bibtex is orphaned, pass CMD check with the following code:
#   # https://stackoverflow.com/questions/61440165/r-check-warning-requires-orphaned-package
#   biblist <- NA
#   if (requireNamespace("bibtex", quietly=TRUE)) {
#     biblist <- bibtex::read.bib(bibfile)
#   } else {
#     cli::cli_alert_warning("Require {.field bibtex} to parse the bibtex")
#   }
#   names(biblist)
# }
