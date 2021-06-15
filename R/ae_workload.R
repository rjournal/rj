#' Check the number of articles an AE is currently working on
#'
#' The function Github REST API to query the ae prefixed repository and extract the content in the submissions folder.
#'
#' @importFrom gh gh
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate count
#' @importFrom tidyr unnest
#' @export
ae_workload <- function(){

  org_repos <- gh("GET /orgs/{org}/repos", org = "rjournal", type = "all")

  repo <- vapply(org_repos, "[[", "", "name")

  ae_repo <- repo[stringr::str_detect(repo, "ae")]

  tibble::tibble(ae = ae_repo) %>%
    dplyr::mutate(folder = list(gh("GET /repos/{owner}/{repo}/contents/{path}",
                                   owner = "rjournal",
                                   repo = ae_repo,
                                   path = "Submissions"))) %>%
    tidyr::unnest(.data$folder) %>%
    dplyr::count(.data$ae)

}
