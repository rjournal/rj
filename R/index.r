#' Parse the index.dcf file.
#'
#' @examples
#' parse_index(sample())
parse_index <- function(path = "index.dcf") {
  dcf <- read.dcf(path,
    fields = c("ID", "Authors", "Title", "Editor", "Reviewers", "Status"))
  colnames(dcf) <- tolower(colnames(dcf))

  n <- nrow(dcf)
  lapply(seq_len(n), function(i) do.call(article, as.list(dcf[i, ])))
}
