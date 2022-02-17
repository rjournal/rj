#' Extract email from leading author of an issue for emailing
#'
#' If the email of leading author is not available, the first author with
#' available email will be used.
#'
#' @param issue the issue number in the `Proofs` folder, i.e. "2021-2"
#'
#' @export
#' @examples
#' \dontrun{
#' author_emails("2021-2")
#' }
author_emails <- function(issue){

  id <- dir(rj:::issue_dir(issue), pattern = "\\d{4}-\\d{2}", full.names = TRUE)

  dt <- purrr::map(id, function(x){
    desc <- rj:::load_article(file.path(x, "DESCRIPTION"))
    Filter(function(x)!is.null(x$email), desc$authors)[[1]]
    })

  out <- purrr::map_dfr(dt, ~tibble(name = .x$name, email = .x$email))
  out
}
