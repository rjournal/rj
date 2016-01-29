#' Download submissions from gmail.
#' 
#' Assumes submissions to be processed are unread emails in the
#' rjournal.submission@gmail.com inbox.  For authorization to work,
#' you must be logged into the rjournal.submission account before
#' calling this function.
#' 
#' @section Process:
#' The function does three things automatically:
#' \enumerate{
#'  \item Downloads and extracts submissions into appropriate directories.
#'  \item Marks submissions as "read" in the inbox.
#'  \item Uploads acknowledgement emails to gmail account as drafts.
#' }
#' The user (editor-in-chief) then:
#' \enumerate{
#'  \item Ensures that the files have unzipped correctly (some authors
#'    incorrectly upload .rar or .tar.gz files) and that the latex
#'    compiles
#'  \item Manually sends the draft emails
#' }
#'
#' @return list of \code{gmail_draft} objects for
#'   \code{\link{send_acknowledgements}}
#' @export
get_submissions <- function() {
    authorize(c("read_only", "modify", "compose"))
    subs <- download_submissions()
    consume_submissions(subs)
    draft_acknowledgements(subs)
}

authorize <- function(scope) {
    secret_file <- system.file("auth", "client_id.json", package="rj")
    gmailr::gmail_auth(scope, secret_file = secret_file)
}

extract_files <- function(files, dest) {
    for (file in files) {
        extractor <- switch(tools::file_ext(file),
                            zip = utils::unzip,
                            gz = utils::untar)
        if (is.null(extractor))
            return(NULL)
        extractor(file, exdir=dest)
        unlink(file)
        unlink(file.path(dest, "__MACOSX"), recursive=TRUE)
        exfiles <- list.files(dest, full.names=TRUE)
        nested <- length(exfiles) == 1L && file.info(exfiles)$isdir
        if (nested) {
            subfiles <- list.files(exfiles, full.names=TRUE)
            file.rename(subfiles, file.path(dest, basename(subfiles)))
            unlink(exfiles, recursive=TRUE)
        }
    }
}

create_submission_directory <- function(id) {
    dir <- file.path("Submissions", format(id))
    dir.create(dir)
    dir
}

as.article.gmail_message <- function(msg, ...) {
    dcf <- read.dcf(textConnection(sub("\\[.*", "", gmailr::body(msg)[[1L]])))
    dcf <- setNames(dcf, tolower(colnames(dcf)))
    do.call(article, c(dcf, list(...)))
}

download_submissions <- function() {
    authorize("read_only")
    msgs <- gmailr::messages("is:unread subject:'R Journal Submission' from:me")
    msgids <- rev(gmailr::id(msgs)) # inbox is sorted latest first
    arts <- lapply(msgids, function(msgid) {
                       id <- new_id()
                       path <- create_submission_directory(id)
                       msg <- gmailr::message(msgid, format="full")
                       files <- gmailr::save_attachments(msg, path=path)
                       extract_files(files, path)
                       art <- as.article.gmail_message(msg, id=id, path=path)
                       save_article(art)
                       art
                  })
    setNames(arts, msgids)
}

consume_submissions <- function(subs) {
    authorize("modify")
    for(msgid in names(subs))
        gmailr::modify_message(msgid, remove_labels="UNREAD")
}

draft_acknowledgements <- function(subs) {
    authorize("compose")
    acknowledge_sub <- function(sub) {
        body <- render_template(sub, "gmail_acknowledge")
        email <- gmailr::mime(From="rjournal.submission@@gmail.com",
                              To=sub$authors[[1L]]$email,
                              Subject=paste("R Journal submission",
                                  format(sub$id)),
                              body=body)
        gmailr::create_draft(email, type="multipart")
    }
    ans <- lapply(subs, acknowledge_sub)
    names(ans) <- vapply(subs, `[[`, "id", FUN.VALUE=character(1L))
    ans
}

#' Send submission acknowledgement drafts
#'
#' @param drafts list of \code{gmail_draft} objects
#' @export
acknowledge_submissions <- function(drafts) {
    for (draft in drafts) {
        gmailr::send_draft(draft)
    }
    for (id in names(drafts)) {
        update_status(id, "acknowledged")
    }
    invisible(TRUE)
}
