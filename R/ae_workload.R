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
  as.data.frame(AE_assignments) %>% count(!!sym("ae"), sort=TRUE)
}

#' @rdname ae_workload
#'
#' @param x An article object
#'
#' @export
get_AE <- function(x){
  list(id = format(x$id), ae = x$ae)
}

#' Add AE to the DESCRIPTION
#'
#' Fuzzy match to find the initial of the AE to fill in the article DESCRIPTION.
#' The status field is also updated with a new line of add AE.
#'
#' @param article article id
#' @param name a name used to match AE. Allow fuzzy match of the whole or part of the AE name, github handle, or email
#' @param date the date for updating status
#' @export
add_ae <- function(article, name, date = Sys.Date()){
  article <- as.article(article)

  ae_list <- read.csv(system.file("associate-editors.csv", package = "rj")) %>%
    mutate(concat = paste0(!!sym("name"), !!sym("github_handle"), !!sym("email")))

  person <- ae_list$github[str_detect(ae_list$concat, name)]
  person_name <- as.character(ae_list$name[str_detect(ae_list$concat, name)])

  if (length(person) != 0){
    # github start with "ae-articles-xxx"
    ae_abbr <- str_sub(person, 13, -1)
    article$ae <- ae_abbr
    update_status(article, "with AE", comments = person_name, date = date)

  } else {
    cli::cli_alert_warning("No AE found. Input the name as the whole or part of the AE name, github handle, or email")
  }

  return(invisible(article))
}
