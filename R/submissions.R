sheet_id <- "15Tem82ikjHhNVccZGrHFVdzVjoxAmwaiNwaBE95wu6k"

#' Download submissions.
#'
#' Obtains submissions from the Google Sheets spreadsheet and downloads
#' submission files from Google Drive.
#' @param dry_run Use TRUE for testing, which will not change the sheet.
#' Default is FALSE.
#'
#' @section Process:
#' The function does three things automatically:
#' \enumerate{
#'  \item Downloads and extracts submissions into appropriate directories.
#'  \item Marks submissions as "read" in the spreadsheet.
#'  \item Uploads acknowledgement emails to gmail account as drafts.
#' }
#'
#' The user (editor-in-chief) then:
#' \enumerate{
#'  \item Ensures that the files have unzipped correctly (some authors
#'    incorrectly upload .rar or .tar.gz files) and that the latex
#'    compiles
#'  \item Manually sends the draft emails
#' }
#' @export
get_submissions <- function(dry_run = FALSE) {
  cli::cat_line("Downloading new submissions")
  subs <- download_submissions(dry_run = dry_run)

  cli::cat_line("Performing automatic checks on submissions")
  # consume_submissions(subs)

  if (!dry_run) {
    cli::cat_line("Drafting acknowledgements")
    draft_acknowledgements(subs)
  }

  if (dry_run) {
    cli::cli_alert_danger("Dry run complete. Check that the submissions have been loaded correctly, and then reset the changes.")
    cli::cli_alert_danger("Do not commit these changes to GitHub, only commit new articles when obtained with `dry_run = FALSE`.")
  }

  invisible(NULL)
}

#' @importFrom gmailr gmail_auth
authorize <- function(scope) {
  secret_file <- system.file("auth", "client_id.json", package = "rj")
  gmailr::gmail_auth(scope, secret_file = secret_file)
}

extract_files <- function(files, dest) {
  for (file in files) {
    extractor <- switch(tools::file_ext(file),
      zip = utils::unzip,
      gz = utils::untar
    )
    if (is.null(extractor)) {
      return(NULL)
    }
    extractor(file, exdir = dest)
    unlink(file)
    unlink(file.path(dest, "__MACOSX"), recursive = TRUE)
    exfiles <- list.files(dest, full.names = TRUE)
    nested <- length(exfiles) == 1L && file.info(exfiles)$isdir
    if (nested) {
      subfiles <- list.files(exfiles, full.names = TRUE)
      file.rename(subfiles, paste0(dest, .Platform$file.sep, basename(subfiles)))
      unlink(exfiles, recursive = TRUE)
    }
  }
}

create_submission_directory <- function(id) {
  dir <- file.path(get_articles_path(), "Submissions", format(id))
  dir_create(dir)
  dir
}

as.article.gmail_message <- function(msg, ...) {
  txt <- sub("\\[.*", "", gmailr::body(msg)[[1L]])
  txt <- gsub("\r\n(\r\n)+", "\n", txt)
  dcf <- read.dcf(textConnection(txt))
  dcf <- setNames(dcf, tolower(colnames(dcf)))
  do.call(article, c(dcf, list(...)))
}

#' @importFrom googlesheets4 read_sheet range_write
download_submissions <- function(dry_run) {
    submissions <- googlesheets4::read_sheet(sheet_id)
    ## the sheet is a nightmare, because the "names" are labels, so
    ## if anyone makes a small change it breaks the whole process.
    ## So please NOT change the labels!
    ## We try to remove some comments and long labels to make it
    ## at least a little more robust.
    names(submissions) <- gsub("(.{10}[^ ]+) .*","\\1",gsub(" ?\\(.*", "", names(submissions)))
  ids <- submissions[["Submission ID"]]
  new_submission <- is.na(ids)
  resub_field <- "If this is a"
  resub_ids <- submissions[[resub_field]][new_submission]
  is_resub <- !is.na(resub_ids)

  # Check that resubmitted articles are not yet rejected (warranting a new ID)
  resub_accepted <- file.exists(file.path(get_articles_path(), "Accepted", resub_ids[is_resub]))
  resub_submitted <- file.exists(file.path(get_articles_path(), "Submissions", resub_ids[is_resub]))
  resub_rejected <- file.exists(file.path(get_articles_path(), "Rejected", resub_ids[is_resub]))
  is_resub[is_resub] <- resub_accepted | resub_submitted

  new_ids <- vector("list", sum(new_submission))

  # Generate IDs
  new_ids[!is_resub] <- future_ids(ids[!new_submission], n = sum(new_submission) - sum(is_resub))
  new_ids[is_resub] <- lapply(resub_ids[is_resub], parse_id)

  new_articles <- submissions[new_submission, ]
  new_articles[["Submission ID"]] <- vapply(new_ids, format, character(1L))
  # sanity check before we fetch things
  bad <- FALSE
  for (form in split(new_articles, new_articles[["Submission ID"]])) {
      id <- form[["Submission ID"]]
      resub <- identical(id, form[[resub_field]])
      cat("ID=",id[1],", n=", length(id), ", ", if(resub) "(resubmission)" else "(new)", "\n", sep='')
      if (length(id) > 1) {
          cat("  multiple submissions: ", as.character(form$Timestamp), "\n")
          form <- form[nrow(form),]
          id <- form[["Submission ID"]]
      }
      if (resub) {
          art <- tryCatch(as.article(id), error=function(e) NULL)
          if (is.null(art)) {
              message(" ** invalid ID **\n");
              bad <- TRUE
          }
      }
  }
  if (bad)
      return(FALSE)
  
  articles <- lapply(
    split(new_articles, new_articles[["Submission ID"]]),
    function(form) {
        ## use only the last entry if multiple are submitted
        if (nrow(form) > 1)
            form <- form[nrow(form),]
      id <- form[["Submission ID"]]
      if (!identical(id, form[[resub_field]])) {
        path <- create_submission_directory(id)

        # If the article is a new submission
        files <- download_submission_file(form[["Upload submission"]], path = path)
        tryCatch(extract_files(files, path),
                 error=function(e) {
                     cli::cli_alert_danger("Error while extracting contents: {e}")
                 })

        # Combine author fields
        authors <- str_glue_data(form, "{`Your name`} <{`Email address`}>")
        other_authors <- form[["Names of other"]]
        if (!is.na(other_authors)) {
          other_authors <- str_trim(str_split(other_authors, ",")[[1]])
          other_authors <- setdiff(other_authors, form[["Your name"]])
          authors <- str_c(c(authors, other_authors), collapse = ", ")
        }

        art <- make_article(
          id = id,
          authors = authors,
          title = form[["Article title"]],
          path = path,
          type = form[["Article type"]],
          suppl = form[["Please list"]] %NA% "",
          keywords = form[["Article tags"]] %NA% "",
          otherids = form[[resub_field]] %NA% ""
        )

        update_status(art, status = "submitted", date = as.Date(form$Timestamp), replace=FALSE)
        cli::cli_alert_success("New submission with ID {id} successfully processed.")
        return(TRUE)
      } else {
        # If the article is a re-submission

        # 1. Get original article
        art <- as.article(id)
        path <- art$path
        path_dir <- basename(dirname(path))
        if (path_dir != "Submissions") {
          cli::cli_alert_warning("Re-submission for {id} is replacing an article in the '{path_dir}' folder!")
        }

        # 2. Check that the metadata matches
        matches_title <- art$title == form$`Article title`
        matches_email <- art$authors[[1]]$email == form$`Email address`
        if (!matches_title && !matches_email) {
          cli::cli_alert_danger("Re-submission for {id} does not match original title and email. Contact {form$`Email address`} to clarify.")
          return(FALSE)
        }

        # 3. Zip old submission into /history
        old_submission <- setdiff(
          list.files(art$path, include.dirs = TRUE),
          c("DESCRIPTION", "history", "correspondence")
        )
        num_submissions <- length(list.files(file.path(path, "history")))
        dir_create(file.path(path, "history"))
        curwd <- setwd(path)
        zip(
          zipfile = file.path("history", paste0(num_submissions + 1, ".zip")),
          files = old_submission,
          flags = "-r9Xmq"
        )
        setwd(curwd)

        # 4. Obtain new submission
        files <- download_submission_file(form[["Upload submission"]], path = path)
        extract_files(files, path)

        # 5. Update article DESCRIPTION
        update_status(art, status = "revision received", date = as.Date(form$Timestamp), replace=FALSE)

        cli::cli_alert_success("Re-submission for ID {id} successfully processed.")
        return(TRUE)
      }
    }
  )
  if (!dry_run) {
    cli::cli_alert_info("Writing new article IDs to Google Sheets.")
    googlesheets4::range_write(sheet_id, new_articles["Submission ID"],
      sheet = "Form responses 1", col_names = FALSE,
      range = str_c(LETTERS[which(names(submissions) == "Submission ID")], range(which(new_submission)) + 1, collapse = ":")
    )
  }
  articles
}

#' Generate a new id value.
#'
#' Inspects submissions/, accepted/ and rejected to figure out which
#' id is next in sequence.
#' @param ids XXX
#' @param n No. of ids to generate
#' @export
future_ids <- function(ids, n = 1) {
  ids <- lapply(ids, parse_id)

  this_year <- Filter(function(x) x$year == year(), ids)

  seqs <- vapply(this_year, function(x) x$seq, integer(1))
  max_seq <- if (is_empty(seqs)) 0 else max(seqs)
  lapply(max_seq + seq_len(n), id, year = year())
}

#' @importFrom stringr str_remove fixed
download_submission_file <- function(url, path = get_articles_path()) {
  file <- googledrive::as_dribble(url)
  path <- path_ext_set(path(path, file$id), path_ext(file$name))
  id <- basename(dirname(path))
  if (file_exists(path)) {
    cli::cli_alert_info("Skipping {basename(path)}, it already exists")
    return(path)
  }
  result <- purrr::safely(googledrive::drive_download)(file, path, verbose = FALSE)
  if (is.null(result$error)) {
    cli::cli_alert_success("{id}: Downloaded {file$id} successfully")
    return(path)
  } else {
    cli::cli_alert_danger("{id}: Failed downloading {file$id} (reason: {result$error$message})")
  }
  return(NA_character_)
}

consume_submissions <- function(subs) {
  for (msgid in names(subs)) {
    gmailr::modify_message(msgid, remove_labels = "UNREAD")
  }
}

#' @importFrom gmailr mime create_draft
draft_acknowledgements <- function(subs) {
  authorize(c("read_only", "modify", "compose"))
  acknowledge_sub <- function(sub) {
    body <- render_template(sub, "gmail_acknowledge")
    email <- gmailr::mime(
      From = "rjournal.submission@@gmail.com",
      To = sub$authors[[1L]]$email,
      Subject = paste(
        "R Journal submission",
        format(sub$id)
      ),
      body = body
    )
    gmailr::create_draft(email)
  }
  ans <- lapply(subs, acknowledge_sub)
  names(ans) <- vapply(subs, function(s) format(s$id),
    FUN.VALUE = character(1L)
  )
  ans
}

#' Send submission acknowledgements
#'
#' @param article this is the article id
#' @param editor optional string, if specified, also sets the \code{Editor:} field to that value and the handling editor will be CCd on the e-mail.
#'
#' @export
acknowledge_submission <- function(article, editor) {
  data <- as.data(a <- as.article(article))
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 5, "%d %b %Y")
  if (!missing(editor)) {
      a$editor <- editor
      save_article(a)
  } else if (length(data$editor))
      editor <- names(data$editor)
  if (!missing(editor)) {
      ed <- read.csv(system.file("editors.csv", package = "rj"), stringsAsFactors = FALSE)
      data$edname <- ed$real[match(editor, ed[[1]])]
      data$edmail <- ed$email[match(editor, ed[[1]])]
  }

  template <- find_template("acknowledge")
  email <- whisker.render(readLines(template), data)

  update_status(data$id, "acknowledged", replace=FALSE)

  email_text(email)
}

#' Send revision received acknowledgement
#'
#' @param article this is the article id
#'
#' @export
acknowledge_revision <- function(article) {
  data <- as.data(as.article(article))
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 5, "%d %b %Y")

  template <- find_template("acknowledge_revision")
  email <- whisker.render(readLines(template), data)

  update_status(data$id, "revision received", replace=FALSE)

  email_text(email)
}

#' Send submission acknowledgement drafts
#'
#' @param drafts list of \code{gmail_draft} objects
#' @importFrom gmailr send_draft
#' @export
draft_acknowledge_submissions <- function(drafts) {
  for (draft in drafts) {
    gmailr::send_draft(draft)
  }
  for (id in names(drafts)) {
    update_status(id, "acknowledged", replace=FALSE)
  }
  invisible(TRUE)
}

#' Create an acknowledgement email in correspondence folder
#'
#' @param article article id
#' @export
acknowledge_submission_text <- function(article) {
  article <- as.article(article)

  dest <- file.path(article$path, "correspondence")
  if (!file.exists(dest)) dir.create(dest)

  name <- "acknowledge.txt"
  path <- file.path(dest, name)

  data <- as.data(article)
  data$name <- stringr::str_split(data$name, " ")[[1]][1]
  data$date <- format(Sys.Date() + 30, "%d %b %Y")

  template <- find_template("acknowledge")
  email <- whisker.render(readLines(template), data)

  writeLines(email, path)

  update_status(data$id, "acknowledged", replace=FALSE)
  invisible(TRUE)
}
