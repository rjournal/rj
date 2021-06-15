#' Check the number of articles an AE is currently working on
#' @export
ae_workload <- function(){
  submitted_articles <- dir(file.path(get_articles_path(), "Submissions"))

  arts <- map(submitted_articles, as.article)

  length(arts)
}
