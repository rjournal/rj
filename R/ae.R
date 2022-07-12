#' Detect user name and e-mail from GIT
#' @return named string, e-mail of the user
git_user <- function() {
  name <- system("git config --get user.name", intern = TRUE)
  email <- system("git config --get user.email", intern = TRUE)
  names(email) <- name
  email
}

#' Associate editor (AE) functions
#'
#' Functions to determine if the user is an AE and retrieve relevant AE information
#'
#' @param path string, path to the git repository to use
#'        for detection.
#' @param require logical, if \code{TRUE} then failing to detect
#'        the AE is considered an error
#'
#' @details
#' \itemize{
#'  \item{\code{AEs()}:  read the associate-editors.csv in the rj package as a data frame}
#'  \item{\code{detect_AE()}: determine AE from the remote of the repository in
#'  \code{path} or from the git config e-mail.this only work for AE and will fail for editors}
#'  \item{\code{AE()}: returns the corresponding row from \code{AEs()} if called
#' by an associate editor or in an AE repository, otherwise
#' \code{NULL}. It relies on either \code{RJ_EDITOR} environment
#' variable to contain the name of the editor or detection from the
#' git repository pointed to by \code{path} (see
#' \code{\link{detect_AE}}).}
#'  \item{\code{is_AE()}: determine if the user is an AE or the repository is an AE repository}
#' }
#' @return
#' \itemize{
#'   \item{\code{AEs()}: a data frame with all associate editors}
#'   \item{\code{detect_AE()}:  \code{NULL} if not found or the
#'        row from \code{AEs()} corresponding to the AE}
#'   \item{	\code{AE}: \code{NULL} if not an AE or a row from \code{AEs()}}
#'   \item{\code{is_AE}: \code{TRUE} if the user if an AE or
#'   the repository is an AE repository, \code{FALSE} otherwise}
#' }
#'
#' @export
AEs <- function() {
  d <- read.csv(system.file("associate-editors.csv", package = "rj"),
    stringsAsFactors = FALSE
  )
  valid <- nzchar(d$github)
  if (any(!valid)) warning("associate-editors.csv contains invalid entries!")
  d[valid, ]
}

#' @rdname AEs
detect_AE <- function(path = ".", require = FALSE) {
  ae <- AEs()
  rem <- suppressWarnings(system(paste("git -C ", shQuote(path.expand(path)), " remote -v 2>&1"), intern = TRUE))
  ## if we can't detect it from the repo, try git config
  m <- if (identical(attr(rem, "status"), 128L)) {
    user <- git_user()
    m <- na.omit(match(tolower(user), tolower(ae$email)))
    seq.int(nrow(ae)) %in% m
  } else {
    sapply(ae$github, function(o) isTRUE(any(grepl(paste0(o, "\\.git"), rem, TRUE))))
  }
  if (require && !any(m)) {
    stop("Could not detect any AE")
  }
  if (any(m)) ae[m, , drop = FALSE] else NULL
}

#' @rdname AEs
#' @export
AE <- function(path = ".") {
  name <- Sys.getenv("RJ_EDITOR")
  if (!nzchar(name)) {
    detect_AE(path, FALSE)
  } else {
    ae <- AEs()
    m <- match(tolower(name), tolower(ae$name))
    if (!is.na(m)) ae[m, , drop = FALSE] else NULL
  }
}

#' @rdname AEs
is_AE <- function(path = ".") length(AE(path)) > 0
