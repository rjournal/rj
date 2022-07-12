#' Find reviewers through keywords matching
#'
#' Find reviewers for an article through matching the article keywords
#' to the keywords reviewers provided when registering.
#' Notice that a googlesheet authenticate, with your email address printed,
#' will first pop up to verify the access to the reviewer googlesheet.
#'
#'   All the reviewers are ranked based on the number of matching keywords
#'   and when there is a tie, a random draw is used.
#'
#'   For example, an article A has 3 keywords. Two reviewers have all the 3 keywords matched,
#'   5 reviewers have 2 matches, and another 10 have 1 match. To get 5 reviewers for article A,
#'   both reviewers with 3 matches are in and a random draw, among the five reviewers with 2 matches,
#'   is used to fill the remaining 3 places.
#'
#' @param id the article id in the description file
#' @param n numeric; number of reviewer to display
#' @examples
#' \dontrun{
#' m1 <- match_keywords("2021-13")
#' m2 <- match_keywords("2021-13", n = 10)
#' }
#'
#' @importFrom rlang .data
#' @export
match_keywords <- function(id, n = 5) {

  article <- get_article_keywords(id)

  # the kw list for article submission & reviewer application is slightly different
  # i.e. graphic is split into two in the article submission form.
  # see the bottom of the file for creating the keyword list
  ctv <- read.csv(system.file("keywords-list.csv", package = "rj"),
                  stringsAsFactors = FALSE)

  ctv_keywords <- ctv %>%
    dplyr::filter(.data$submission %in% article$keywords) %>%
    dplyr::pull(.data$reviewer)
  other_keywords <- article$keywords[!article$keywords %in% ctv$submission] %>%
    stringr::str_to_title()

  reviewer <- get_reviewer_keywords()
  ae <- AEs()
  # remove all the AE and authors from the potential reviewer
  reviewer <- reviewer %>%
    dplyr::filter(!.data$fname %in% article$author,!.data$email %in% ae$email)

  potential <- reviewer %>%
    dplyr::filter(keywords %in% c(ctv_keywords, other_keywords))

  total <- n # since n conflicts with the n column from count
  meta <- potential %>%
    dplyr::count(keywords) %>%
    dplyr::mutate(msg = glue::glue("{keywords} ({n})")) %>%
    dplyr::mutate(weight = allocate_reviewer(n, n = total)) %>%
    dplyr::arrange(weight)

  cli::cli_alert_info("Reviewer found for each standardised keyword: [keywords (n_selected/ total): selected]")
  out <- vector(); i <- 1
  for (i in seq_len(nrow(meta))){
    this_round <- match_single(df = potential,
                               keywords = meta$keywords[i],
                               n = meta$weight[i],
                               already = out)
    r <- paste0(this_round, collapse = ", ")
    cli::cli_alert_info(
      glue::glue("{meta$keywords[i]} ({length(this_round)}/ {meta$n[i]}): {r}"))

    out <- out %>% append(this_round)
    i <- i + 1
  }

    reviewer %>%
      dplyr::filter(.data$fname %in% out) %>%
      dplyr::arrange(factor(.data$fname, levels = out)) %>%
      dplyr::group_by(gname, fname, email) %>%
      dplyr::summarise(keywords = list(keywords)) %>%
      dplyr::ungroup()


}

## --------------------
## helper
allocate_reviewer <- function(vec, n){
  out <- ceiling(vec/sum(vec) * n)

  vec_order <- order(vec, decreasing = TRUE); i <- 1
  while(sum(out) != n){
    idx <- vec_order[i]
    out[idx] <- out[idx] - 1
    i <- i + 1
  }

  out
}

match_single <- function(df, keywords, n, already){

  selected <- df %>% dplyr::filter(keywords == keywords) %>% dplyr::pull(email)

  match_list <- df %>%
    dplyr::filter(email %in% selected, !fname %in% already) %>%
    dplyr::group_by(.data$fname) %>%
    dplyr::tally(sort = TRUE)


  if (nrow(match_list) == 0) {
    cli_alert_info(
      "At least one keyword specified by the authors needs to be from the CRAN Task View, but none of keywords is. No match returned."
    )
  } else{
    i <- 0; out <- vector()
    while (length(out) < n) {
      matched <- match_list %>% dplyr::filter(n == max(n) - i)
      n_space <- n - length(out)

      if (n_space >= nrow(matched)) {
        out <- c(out, matched$fname)
        #cli_alert_info("{length(matched$fname)} reviewer{?s} with {unique(matched$n)} matc{?h/hes}")
      } else{
        out <- c(out, sample(matched$fname, n_space))
        # cli_alert_info("Randomly select {n_space} from {nrow(matched)} reviewer{?s} with {unique(matched$n)} matc{?h/hes}")
      }

      i <- i + 1
    }

    return(out)

  }
}


#' Extract keywords from a submitted article
#' @param id the article id
#'
#' @return a list with keywords and authors of the article
get_article_keywords <- function(id) {
  article <- as.article(id)
  keywords_raw <- article$keywords
  author <- unlist(article$author)
  if (nchar(keywords_raw) == 0) {
    rlang::abort("no keyword detected in the DESCRIPTION file!")
  }

  ctr_str <- "Clinical Trial Design, Monitoring, and Analysis"
  has_clinical_trial <- str_detect(keywords_raw, ctr_str)

  if (has_clinical_trial){
    keywords_raw <- str_remove(keywords_raw, ctr_str)
  }

  keywords <- as.vector(stringr::str_split(keywords_raw, ", ", simplify = TRUE))

  if(has_clinical_trial) keywords <- c(ctr_str, keywords)

  list(
    keywords = keywords,
    author = author[names(author) == "name"]
  )
}

#' Extract keywords from reviewer list
get_reviewer_keywords <- function() {
  cli::cli_alert_info("Select the email adress having access to the reviewer googlesheet (if applicable):  ")
  reviewer_info <- read_reviewer_sheet()

  dup <- reviewer_info %>%
    dplyr::mutate(dup = duplicated(email)) %>%
    filter(dup) %>%
    dplyr::pull(email)

  fix_dup <- reviewer_info %>%
    dplyr::filter(email %in% dup) %>%
    dplyr::group_by(email) %>%
    dplyr::filter(dplyr::row_number() == max(dplyr::row_number()))

  dplyr::bind_rows(reviewer_info %>% filter(!email %in% dup), fix_dup) %>%
    tidyr::separate_rows(.data$keywords, sep = ", ") %>%
    mutate(keywords = stringr::str_to_title(.data$keywords))
}

read_reviewer_sheet <- function(){
  sheet_raw <- suppressMessages(googlesheets4::read_sheet(reviewer_sheet_url))
  colnames(sheet_raw) <- c("timestamp", "email","gname","fname",
                           "website","github","twitter","keywords",
                           "blank1", "blank2", "review_completed", "comments")
  sheet_raw
}

reviewer_sheet_url <- "https://docs.google.com/spreadsheets/d/1stC58tDHHzjhf63f7PhgfiHJTJkorvAQGgzdYL5NTUQ/edit?ts=606a86e4#gid=1594007907"

###################################
# creating the keyword matching list
reviewer <- tibble::tribble(
  ~reviewer, ~details,
  "Bayesian", "Bayesian Inference",
  "ChemPhys", "Chemometrics and Computational Physics",
  "ClinicalTrials", "Clinical Trial Design Monitoring and Analysis",
  "Cluster", "Cluster Analysis & Finite Mixture Models",
  "Databases", "Databases with R",
  "DifferentialEquations", "Differential Equations",
  "Distributions", "Probability Distributions",
  "Econometrics", "Econometrics",
  "Environmetrics", "Analysis of Ecological and Environmental Data",
  "ExperimentalDesign", "Design of Experiments (DoE) & Analysis of Experimental Data",
  "ExtremeValue", "Extreme Value Analysis",
  "Finance", "Empirical Finance",
  "FunctionalData", "Functional Data Analysis",
  "Genetics", "Statistical Genetics",
  "Graphics", "Graphic Displays & Dynamic Graphics & Graphic Devices & Visualization",
  "Graphics", "Graphic Displays & Dynamic Graphics & Graphic Devices & Visualization",
  "HighPerformanceComputing", "High-Performance and Parallel Computing with R",
  "Hydrology", "Hydrological Data and Modeling",
  "MachineLearning", "Machine Learning & Statistical Learning",
  "MedicalImaging", "Medical Image Analysis",
  "MetaAnalysis", "Meta-Analysis",
  "MissingData", "Missing Data",
  "ModelDeployment", "Model Deployment with R",
  "Multivariate", "Multivariate Statistics",
  "NaturalLanguageProcessing", "Natural Language Processing",
  "NumericalMathematics", "Numerical Mathematics",
  "OfficialStatistics", "Official Statistics & Survey Methodology",
  "Optimization", "Optimization and Mathematical Programming",
  "Pharmacokinetics", "Analysis of Pharmacokinetic Data",
  "Phylogenetics", "Phylogenetics, Especially Comparative Methods",
  "Psychometrics", "Psychometric Models and Methods",
  "ReproducibleResearch", "Reproducible Research",
  "Robust", "Robust Statistical Methods",
  "SocialSciences", "Statistics for the Social Sciences",
  "Spatial", "Analysis of Spatial Data",
  "SpatioTemporal", "Handling and Analyzing Spatio-Temporal Data",
  "Survival", "Survival Analysis",
  "TeachingStatistics", "Teaching Statistics",
  "TimeSeries", "Time Series Analysis",
  "Tracking", "Processing and Analysis of Tracking Data",
  "WebTechnologies", "Web Technologies and Services",
  "gR", "gRaphical Models in R"
) %>%
  mutate(reviewer = gsub("([A-Z])", " \\1", reviewer),
         reviewer = stringr::str_trim(reviewer))

submission <-
  tibble::tribble(
    ~submission,
    "Bayesian Inference",
    "Chemometrics and Computational Physics",
    "Clinical Trial Design Monitoring and Analysis",
    "Cluster Analysis & Finite Mixture Models",
    "Databases with R",
    "Differential Equations",
    "Distributions",
    "Econometrics",
    "Ecological and Environmental analysis",
    "Design and Analysis of Experiments (DoE)",
    "Extreme Value Analysis",
    "Empirical Finance",
    "Functional Data Analysis",
    "Statistical Genetics",
    "Graphical Models",
    "Graphics and Visualisation",
    "High-Performance Computing",
    "Hydrological Data and Modeling",
    "Machine Learning & Statistical Learning",
    "Medical Image Analysis",
    "Meta-Analysis",
    "Missing Data",
    "Model Deployment",
    "Multivariate Statistics",
    "Natural Language Processing",
    "Numerical Mathematics",
    "Official Statistics & Survey Methodology",
    "Optimization and Mathematical Programming",
    "Pharmacokinetic Analysis",
    "Phylogenetics",
    "Psychometric Models and Methods",
    "Reproducible Research",
    "Robust Statistical Methods",
    "Social Sciences",
    "Spatial Analysis",
    "Spatio-Temporal Analysis",
    "Survival Analysis",
    "Teaching Statistics",
    "Time Series Analysis",
    "Tracking Data",
    "Web Technologies and Services",
    "gRaphical Models in R",
  )

keywords_list <- cbind(reviewer, submission) %>% tibble::as_tibble()
#write.csv(keywords_list, file = "inst/keywords-list.csv")
