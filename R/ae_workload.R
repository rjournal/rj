#' Check the number of articles an AE is currently working on
#'
#' This will examine the DESCRIPTION files for articles in
#' the Submissions folder, check articles that have status
#' "with AE".
#'
#' @importFrom dplyr count
#' @importFrom cli cli_li cli_ul cli_end
#' @export
ae_workload <- function() {
  # Get list of current articles
  current_articles <- active_articles()

  # Check that the most recent status is "with AE"
  with_AE <- filter_status(current_articles, "with AE")

  # Extract the AE line from DESCRIPTION file
  AE_assignments <- do.call("rbind", lapply(with_AE, get_AE))

  # Count assignments
  as.data.frame(AE_assignments) %>% count(ae, sort=TRUE)
}

#' @rdname ae_workload
#' @export
get_AE <- function(x){
  list(id = format(x$id), ae = x$ae)
}
