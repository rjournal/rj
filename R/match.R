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
#' match_keywords("2021-13")
#' match_keywords("2021-13", n = 10)
#' }
#'
#' @importFrom rlang .data
#' @export
match_keywords <- function(id, n = 5) {

  article <- get_article_keywords(id)

  # the kw list for article submission & reviewer application is slightly different
  # i.e. graphic is split into two in the article submission form.
  # see the bottom of the file for creating the keyword list
  kw_fm_article <- read.csv(system.file("keywords-list.csv", package = "rj"),
                            stringsAsFactors = FALSE) %>%
    dplyr::filter(.data$submission %in% article$keywords) %>%
    dplyr::pull(.data$reviewer)

  reviewer <- get_reviewer_keywords()
  ae <- AEs()

  # remove all the AE and authors from the potential reviewer
  reviewer_kw <- reviewer %>%
    tidyr::separate_rows(.data$keywords, sep = ", ") %>%
    filter(
      !.data$fname %in% article$author,
      !.data$email %in% ae$email
    )

  match_list <- reviewer_kw %>%
    filter(keywords %in% kw_fm_article) %>%
    dplyr::group_by(.data$fname) %>%
    dplyr::tally(sort = TRUE)


  if (nrow(match_list) == 0) {
    cli_alert_info(
      "At least one keyword specified by the authors needs to be from the CRAN Task View, but none of keywords is. No match returned."
    )
  } else{
    out <- vector()
    i <- 0
    while (length(out) < n) {
      matched <- match_list %>% dplyr::filter(n == max(n) - i)
      n_space <- n - length(out)

      if (n_space >= nrow(matched)) {
        out <- c(out, matched$fname)
        cli_alert_info("{length(matched$fname)} reviewer{?s} with {unique(matched$n)} matc{?h/hes}")
      } else{
        out <- c(out, sample(matched$fname, n_space))
        cli_alert_info(
          "Randomly select {n_space} from {nrow(matched)} reviewer{?s} with {unique(matched$n)} matc{?h/hes}"
        )
      }

      i <- i + 1
    }


    reviewer %>%
      dplyr::filter(.data$fname %in% out) %>%
      dplyr::arrange(factor(.data$fname, levels = out))
  }


}

## --------------------
## helper

#' Extract keywords from a submitted article
#' @param id the article id
#'
#' @return
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
#' @return
get_reviewer_keywords <- function() {
  sheet_raw <- suppressMessages(googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1stC58tDHHzjhf63f7PhgfiHJTJkorvAQGgzdYL5NTUQ/edit?ts=606a86e4#gid=1594007907"))
  reviewer_info <- tibble::tibble(
    gname = sheet_raw$`What's your given name, eg how you would like to be addressed (eg Mike)?`,
    fname = sheet_raw$`What's your full name (eg Michael Kane)?`,
    email = sheet_raw$`Email address`,
    keywords = sheet_raw$`Please indicate your areas of expertise, check as many as you feel are appropriate.  (Based on available CRAN Task Views.)`
  )

  dup <- reviewer_info %>%
    dplyr::mutate(dup = duplicated(email)) %>%
    filter(dup) %>%
    dplyr::pull(email)

  fix_dup <- reviewer_info %>%
    dplyr::filter(email %in% dup) %>%
    dplyr::group_by(email) %>%
    dplyr::filter(dplyr::row_number() == max(dplyr::row_number()))

  dplyr::bind_rows(reviewer_info %>% filter(!email %in% dup),
                   fix_dup)
}

####################################
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

keywords_list <- cbind(reviewer, submission)
#write.csv(keywords_list, file = "inst/keywords-list.csv")



