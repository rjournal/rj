#' Get path to sample dataset.
#'
#' @keywords internal
#' @export
sample <- function() {
  system.file("index.dcf", package = "rj")
}
