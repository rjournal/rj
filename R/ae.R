#' Detect user name and e-mail from GIT
#' @return named string, e-mail of the user
git_user  <- function() {
    name <- system("git config --get user.name", intern=TRUE)
    email <- system("git config --get user.email", intern=TRUE)
    names(email) <- name
    email
}

#' @title Associate editor (AE) functions
#'
#' @description
#' \code{AEs()} returns a data frame with information on
#' associate editors
#'
#' @return \code{AEs} returns a data frame with all associate editors
#' @export
AEs <- function()
    read.csv(system.file("associate-editors.csv", package = "rj"),
             stringsAsFactors=FALSE)

#' @description
#' \code{detect_AE} tries to determine associate editor (AE) from the
#' remote of the repository in \code{path} or from the git config e-mail.
#' Note that for editors this will fail.
#'
#' @param path string, path to the git repository to use
#'        for detection.
#' @param require logical, if \code{TRUE} then failing to detect
#'        the AE is considered an error
#' @return \code{detect_AE} returns \code{NULL} if not found or the
#'        row from \code{AEs()} corresponding to the AE
#' @rdname AEs
detect_AE <- function(path=".", require=FALSE) {
    ae <- AEs()
    rem <- suppressWarnings(system(paste("git -C ", shQuote(path.expand(path)), " remote -v 2>&1"), intern=TRUE))
    ## if we can't detect it from the repo, try git config
    m <- if (identical(attr(rem, "status"), 128L)) {
        user <- git_user()
        m <- na.omit(match(tolower(user), tolower(ae$email)))
        seq.int(nrow(ae)) %in% m
    } else 
        sapply(ae$github, function(o) isTRUE(any(grepl(paste0(o, "\\.git"), rem, TRUE))))
    if (require && !any(m))
        stop("Could not detect any AE")
    if (any(m)) ae[m,,drop=FALSE] else NULL
}

#' @description
#' \code{AE} returns the corresponding row from \code{AEs()} if called
#' by an associate editor or in an AE repository, otherwise
#' \code{NULL}. It relies on either \code{RJ_EDITOR} environment
#' variable to contain the name of the editor or detection from the
#' git repository pointed to by \code{path} (see
#' \code{\link{detect_AE}}).
#'
#' @return \code{AE} returns \code{NULL} if not an AE or a row from
#'         \code{AEs()}
#'
#' @rdname AEs
#' @export
AE <- function(path=".") {
    name <- Sys.getenv("RJ_EDITOR")
    if (!nzchar(name))
        detect_AE(path, FALSE)
    else {
        ae <- AEs()
        m <- match(tolower(name), tolower(ae$name))
        if (any(m)) ae[m,,drop=FALSE] else NULL
    }
}

#' @description \code{is_AE()} is a shorthand for \code{!is.null(AE())}
#' @return \code{is_AE} returns \code{TRUE} if the user if an AE or
#'         the repository is an AE repository, \code{FALSE} otherwise
#'
#' @rdname AEs
is_AE <- function(path=".") length(AE(path)) > 0
