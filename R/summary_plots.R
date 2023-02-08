
#' Generates a status plot for articles submitted in the last few years.
#'
#' @param nyears The number of years considered. Excludes current year.
#' @param save Defaults to TRUE. The plot is saved in the rjournal.github.io/resources folder.
#'
#' @return a ggplot, one bar per year (taken from article id)
#' @export
#'
#' @examples
#' \dontrun{
#' article_status_plot()
#' }
article_status_plot <- function(nyears=4, save=TRUE){
  this_year <- as.numeric(format(Sys.Date(), "%Y"))
  sel_years <- as.character(this_year - (1:nyears))

  # similar to tabulate_articles, excepted added year, as some earlier articles have malformed DESCRIPTION files
  tabulate_articles_year<-
    function (dirs = c("Accepted", "Submissions"), id_year=NULL)  {
      ids <- article_ids(dirs)
      if (is.character(id_year))
        ids <- ids[substr(ids,1,4) %in% id_year]
      purrr::map_dfr(ids, tabulate_single)
    }


  subs <- tabulate_articles_year("Submissions", sel_years ) |>
    select(id,  status) |>
    unnest(status) |>
    group_by(id) |>
    dplyr::slice_tail() |>
    mutate(current="in progress")

  rej <- tabulate_articles_year("Rejected",sel_years) |>
    select(id,  status) |>
    unnest(status) |>
    group_by(id) |>
    dplyr::slice_tail() |>
    mutate(current="rejected")

  acc <- tabulate_articles_year(c( "Accepted", "Proofs"),sel_years )|>
    select(id, status) |>
    unnest(status) |>
    group_by(id) |>
    dplyr::slice_tail() |>
    mutate(current="accepted/published")

  allart <- bind_rows(acc, subs, rej) |>
    mutate(year = substr(id,1,4)) |> ungroup()

  g <- ggplot2::ggplot(allart, ggplot2::aes(x=year, fill=.data$current))+
    ggplot2::geom_bar() + ggplot2::xlab("Submission year")
  if (save) {
    fn <- file.path(normalizePath("../rjournal.github.io/resources", mustWork = TRUE), "article_status_plot.png")
    ggplot2::ggsave(fn,g,device="png",width = 4, height = 3)
    print(paste("Save to", fn))
  }
  g

}


#' Generates a plot of acceptance times for articles published in the last few years.
#'
#' @param nyears The number of years considered. Excludes current year.
#' @param save Defaults to TRUE. The plot is saved in the rjournal.github.io/resources folder.
#'
#' @return a ggplot, one boxplot per publication year
#' @export
#'
#' @examples
#' \dontrun{
#'  time_to_accept_plot()
#' }
#'
time_to_accept_plot <- function(nyears=4, save=TRUE){
  this_year <- as.numeric(format(Sys.Date(), "%Y"))
  sel_years <- as.character(this_year - (1:nyears))

  proof_folders <-dir(file.path(get_articles_path(),"Proofs"))

  proof_folders <-
    file.path("Proofs",
              proof_folders[substr(proof_folders,1,4) %in% sel_years])

  # slug is not pub_year, always, so need to get pub_year from folder

  published <-
    map(proof_folders, tabulate_articles) |>
    bind_rows(.id ="index") |>
    mutate(pub_year = substr(proof_folders[as.numeric(.data$index)],8,11))


  # some dates are messed up, or not present, so this version of code accounts for that
  # also no submitted line, multiple accepted lines

  submitted_info <-
    published  |>
    select(id,  status,  .data$pub_year) |>
    unnest(status) |>
    filter(status =="submitted") |>
    group_by(id)  |>
    dplyr::slice_min(date)

  accepted_info <-
    published  |>
    select(id,  status,  .data$pub_year) |>
    unnest(status) |>
    filter(status =="accepted") |>
    group_by(id)  |>
    dplyr::slice_max(date, with_ties=FALSE)

  published_all <-
    dplyr::full_join(submitted_info, accepted_info, by="id") |>
    mutate(pub_year = max(.data$pub_year.x, .data$pub_year.y, na.rm=T),
           days = dplyr::case_when(
             is.na(date.x) | is.na(date.y) ~ NA,
             date.y >= date.x ~ as.numeric(date.y- date.x),
             TRUE ~ NA
           ))
  g <- ggplot2::ggplot(published_all, ggplot2::aes(x=.data$pub_year, y=.data$days))+
    ggplot2::geom_boxplot(color="navy")+ ggplot2::xlab("Year of publication")+
    ggplot2::ylab("Days from submission to acceptance")

  if (save) {
    fn <- file.path(normalizePath("../rjournal.github.io/resources", mustWork = TRUE), "time_to_accept_plot.png")
    ggplot2::ggsave(fn,g,device="png",width = 4, height = 3)
    print(paste("Save to", fn))
  }
  g
}
