#' Generate a summary of current and recent assignments to each editor
#'
#' This should be run weekly.
#'
#' @export
assignments <- function() {
  # Grab all articles
  rejected <- .parse.articles("Rejected")
  accepted <- .parse.articles("Accepted")
  submissions <- .parse.articles("Submissions")
  # Remove resubmissions
  resubmit <- lapply(submissions, function(u) last_status(u)$status == "resubmission") |>
    unlist()
  submissions[resubmit] <- NULL

  # Calculate summary by editor
  active <- lapply(submissions, function(u) u$editor) |> unlist()
  active <- table(active) |> tibble::as_tibble()
  active$active[active$active == ""] <- "Unassigned"
  active <- active |>
    tidyr::pivot_wider(names_from = "active", values_from = "n") |>
    dplyr::mutate(Assignment = "Active")

  # Remove articles not submitted in last year
  all_articles <- c(rejected, accepted, submissions)
  submitted <- lapply(all_articles, function(u) u$status[[1]]$date) |>
    unlist() |> as.Date()
  all_articles <- all_articles[submitted > Sys.Date() - 365]
  submitted <- submitted[submitted > Sys.Date() - 365]
  last3 <- submitted > Sys.Date() - 90
  last1 <- submitted > Sys.Date() - 30
  # Replace missing editors
  editors <- lapply(all_articles, function(u) u$editor) |> unlist()
  editors[editors == "" & submitted < as.Date("2024-12-31")] <- "MV"
  editors[editors == "" & submitted >= as.Date("2025-01-01")] <- "RH"
  # Calculate summary
  last12 <- table(editors) |>
    tibble::as_tibble() |>
    tidyr::pivot_wider(names_from = "editors", values_from = "n") |>
    dplyr::mutate(Assignment = "Last 12 months")
  last3 <- table(editors[last3], dnn = "editors") |>
    tibble::as_tibble() |>
    tidyr::pivot_wider(names_from = "editors", values_from = "n") |>
    dplyr::mutate(Assignment = "Last 3 months")
  last1 <- table(editors[last1], dnn = "editors") |>
    tibble::as_tibble() |>
    tidyr::pivot_wider(names_from = "editors", values_from = "n") |>
    dplyr::mutate(Assignment = "Last month")

  output <- dplyr::bind_rows(active, last12, last3, last1) |>
    dplyr::select(Assignment, everything())
  output[is.na(output)] <- 0
  output <- as.data.frame(output)
  rownames(output) <- output$Assignment
  output$Assignment <- NULL
  return(output)
}
