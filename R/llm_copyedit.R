#' Copyedit article files using an LLM
#'
#' This function applies LLM-based copyediting to all relevant source files in
#' an article directory. It will process BibTeX files, R Markdown files, and
#' LaTeX files (the latter only if there are no R Markdown files).
#' OpenAI's ChapGPT 4.1 is used with the `ellmer` package to perform the copyediting.
#' You will need an OpenAI API key set in the environment variable `OPENAI_API_KEY`.
#' The changes are made directly to the files. Please check carefully before
#' committing them to the git repository.
#'
#' @param articles A vector of article ids, such as "2025-42". Ignored if issue
#' is provided.
#' @param issue An issue id, such as "2025-1". If provided, the function will
#' construct the vector of associated article ids.
#' @param type Type of files to copyedit. Options are "all" (default), "bib", "tex", or "rmd".
#' @param ellmer_timeout_s Timeout for the LLM API in seconds. Default is 600 seconds.
#' @examples
#' \dontrun{
#' llm_copyedit("2025-05")
#' }
#'
#' @export
#'
llm_copyedit <- function(
  articles = NULL,
  issue = NULL,
  type = c("all", "bib", "tex", "rmd"),
  ellmer_timeout_s = 600
) {
  type <- match.arg(type)
  options(ellmer_timeout_s = ellmer_timeout_s)
  if (!is.null(issue)) {
    if (!is.character(issue) || length(issue) != 1) {
      stop("Issue must be a single character string, e.g., '2025-1'.")
    }
    articles <- list.files(
      file.path(get_articles_path(), "Proofs", issue),
      pattern = "\\d{4}-\\d{2}",
      full.names = TRUE
    )
    articles <- basename(articles)
  } else if (is.null(articles)) {
    stop("Please provide either article ids or an issue id")
  }
  for (article in articles) {
    article <- as.article(article)
    message(paste("Processing article:", article$id))
    llm_copyedit_article(article, type)
  }
}

llm_copyedit_article <- function(article, type) {
  bibfiles <- rmdfiles <- texfiles <- character(0)
  if (type %in% c("all", "bib")) {
    bibfiles <- list.files(
      article$path,
      pattern = "\\.bib$",
      recursive = TRUE,
      full.names = TRUE
    )
    bibfiles <- bibfiles[!grepl("/history/", bibfiles)]
  }
  if (type %in% c("all", "rmd")) {
    rmdfiles <- list.files(
      article$path,
      pattern = "\\.Rmd$",
      recursive = TRUE,
      full.names = TRUE
    )
    rmdfiles <- rmdfiles[basename(rmdfiles) != "RJwrapper.Rmd"]
    rmdfiles <- rmdfiles[!grepl("/history/", rmdfiles)]
  }
  if (type %in% c("all", "tex") & length(rmdfiles) == 0) {
    texfiles <- list.files(
      article$path,
      pattern = "\\.tex$",
      recursive = TRUE,
      full.names = TRUE
    )
    texfiles <- texfiles[basename(texfiles) != "RJwrapper.tex"]
    texfiles <- texfiles[!grepl("/history/", texfiles)]
  }
  if (length(bibfiles) > 0) {
    llm_copyedit_files(
      bibfiles,
      system_prompt = "Make minimal changes to fix common bibtex issues, including syntax errors, duplicate entries, incorrect entry types, trailing commas. Use accent notation. Do not change keys. Do not change author names except where the author is R Core Team when you can replace it with {R Core Team}. Do not leave comments in the bib file. Return only the corrected bibtex content. The rest of these instructions refer only to title fields and booktitle fields. In these fields, and only in these fields, capitalization of a word or phrase is protected by wrapping it in braces. For example, in the title {Travels in Australia with Fred Bloggs}, we need to protect the capitalization of the proper nouns by writing it as {Travels in {Australia} with {Fred Bloggs}}. Do not change capitalization in titles, and do not remove protected capitalization (i.e., words or phrases wrapped in braces). If a word is in all uppercase, protect its capitalization by wrapping it in braces, unless it is already wrapped in braces. Protect proper nouns and names in titles by wrapping them in braces (e.g., replace R with {R}, C++ with {C++}, United States with {United States}, Bayesian with {Bayesian}, Cox with {Cox}, etc.), unless they are already wrapped in braces. Non-standard capitalization occurs when a word contains a mix of upper and lowercase letters, and at least one uppercase letter that is not at the start of the word. Protect any words with non-standard capitalization by wrapping them in braces, unless they are already wrapped in braces. If a title starts with a lower case letter, protect the capitalization of the initial word by wrapping it in braces. For example, the title {forecast: A package to do forecasting} should be written as {{forecast}: A package to do forecasting}."
    )
  }
  if (length(rmdfiles) > 0) {
    llm_copyedit_files(
      rmdfiles,
      system_prompt = "Make minimal copy-editing changes to this R Markdown document to fix critical grammatical errors. Only fix errors, do not make other changes. In particular, do not substitute words, change code, comments, indentation, layout, and file paths. Do not add, remove or change any line breaks or empty lines anywhere in the document. Do not insert blank lines. Do not remove blank lines. Do not insert additional blank lines between paragraphs. Do not insert blank lines between items in a bulleted or enumerated list. Do not insert blank lines in markdown tables. The yaml header is at the top of the document and begins with '---' and ends with '---,' each on a separate line. Do not insert blank lines between items in the yaml header. Equation environments may start with '$$' and end with '$$'. They may also start with '\\begin{equation}' and end with '\\end{equation}'. Or they may start with '\\[' and end with '\\]'. Do not modify these equation environments in any way. Code chunks are sections that begin with '```{r' and end with '```', each at the start of a line. Do not change these code chunks in any way. Output the entire document with only the minimal grammatical corrections applied inline, otherwise preserving the original structure and formatting of the document. Your job is only to fix errors, not to make other changes. If there is any doubt, don't make the change. In summary: 1. identify critical grammatical errors only; 2. Correct only those errors inline; 3. Do not change any other content, formatting, or structure of the document; 4. Do not add or remove line breaks or empty lines anywhere in the document. 5. Do not change code chunks or equation environments in any way."
    )
  }
  if (length(texfiles) > 0) {
    llm_copyedit_files(
      texfiles,
      system_prompt = "Make minimal copy-editing changes to this LaTeX document to fix critical grammatical errors. Only fix errors, do not make other changes. In particular, do not substitute words, change code, comments, indentation, layout, and file paths. Do not add, remove or change any line breaks or empty lines anywhere in the document. Do not insert blank lines. Do not remove blank lines. Do not insert additional blank lines between paragraphs. Do not insert blank lines between items in a bulleted or enumerated list. Do not insert blank lines in tables. Equation environments may start with '$$' and end with '$$'. Or they may start with '\\[' and end with '\\]'. They may also start with '\\begin{equation}' and end with '\\end{equation}'. Do not modify these equation environments in any way. Code chunks are sections that begin with '\\begin{example}` and end with '\\end{example}', each on a new line. Do not change these code chunks in any way. Output the entire document with only the minimal grammatical corrections applied inline, otherwise preserving the original structure and formatting of the document. Your job is only to fix errors, not to make other changes. If there is any doubt, don't make the change. In summary: 1. identify critical grammatical errors only; 2. Correct only those errors inline; 3. Do not change any other content, formatting, or structure of the document; 4. Do not add or remove line breaks or empty lines anywhere in the document. 5. Do not change code chunks or equation environments in any way."
    )
  }
  invisible()
}

llm_copyedit_files <- function(files, system_prompt) {
  chat <- ellmer::chat_openai(
    model = "gpt-5.1",
    system_prompt = system_prompt,
    echo = "none"
  )
  lapply(files, function(file) {
    message(paste("  *", basename(file)))
    content <- remove_empty_lines(xfun::read_utf8(file))
    output <- chat$clone()$chat(content)
    clean_output <- strsplit(output, "\\n\\n")[[1L]]
    xfun::write_utf8(clean_output, file)
  })
}

# Replace multiple empty lines with a single empty line
remove_empty_lines <- function(x) {
  is_empty <- grepl("^\\s*$", x)
  runs <- rle(is_empty)
  runs$lengths[runs$values & runs$lengths > 1] <- 1
  new_is_empty <- inverse.rle(runs)
  result <- character(length(new_is_empty))
  result[!new_is_empty] <- x[!is_empty][cumsum(!new_is_empty)[!new_is_empty]]
  return(result)
}
