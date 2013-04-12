get_submissions <- function() {
  subs <- submissions()
  lapply(subs, get_submission)

  message("Please mark new entries as processed")
  browseURL("https://hadley.wufoo.com/entries/r-journal-submission/")
}

#' Download submissions from WUFOO.
#' @importFrom httr authenticate GET content
submissions <- function() {
  api_url <- "https://hadley.wufoo.com/api/v3"
  submissions <- file.path(api_url, "forms/26/entries.json")

  key <- Sys.getenv("WUFOO_API_KEY")
  auth <- authenticate(key, "ignored")

  resp <- GET(submissions, query = "Filter1=221+Does_not_contain+yes", config = auth)
  new <- content(resp, as = "parsed")[[1]]
  lapply(new, parse_wufoo)
}

parse_wufoo <- function(x) {
  to_change <- names(x) %in% names(fix_names)
  names(x)[to_change] <- fix_names[names(x)[to_change]]

  x$zip_url <- str_match(x$download, "\\((.*?)\\)")[2]
  x
}
fix_names <- c(
  "Field9" = "name",
  "Field11" = "email",
  "Field13" = "authors",
  "Field15" = "title",
  "Field17" = "download",
  "Field19" = "read_instructions",
  "Field25" = "one_col",
  "Field20" = "sep_bib",
  "Field24" = "named",
  "Field21" = "license",
  "Field22" = "not_published",
  "Field23" = "all_authors",
  "Field119" = "extra_packages",
  "Field221" = "processed"
)


get_submission <- function(subm) {
  id <- format(new_id())
  dest <- file.path("Submissions", id)

  message("Downloading submission ", subm$EntryId, " to ", id)
  temp_home <- file.path(tempdir(), paste0(id, ".zip"))
  download(subm$zip_url, temp_home, quiet = TRUE)
  on.exit(unlink(temp_home))

  message("Unzipping and creating DESCRIPTION")
  unzip(temp_home, exdir = dest, setTimes = TRUE)
  writeLines(toJSON(subm), file.path(dest, "submission.json"))
  write_description(dest, subm)
}

write_description <- function(path, subm) {
  authors <- setdiff(str_trim(str_split(subm$authors, ",")[[1]]), subm$name)

  desc <- list(
    Title = subm$title,
    Authors = paste(c(
        paste0(subm$name, " <", subm$email, ">"),
        authors),
      collapse = ",\n"),
    Editor = "",
    Reviewers = "",
    Status = format(c(
      status("Recieved", as.Date(subm$DateCreated)),
      status("Acknowledged")
    ))
  )
  write.dcf(desc, file.path(path, "DESCRIPTION"), keep.white = names(desc))

}

# Find new article id
#
# Assume existing forms (greater than x) are already processed
#
# For each new entry (after id 14):
# http://www.wufoo.com/docs/api/v3/entries/get/
#
#  * Download json into submission.json
#
#  * Parse json & initialise DESCRIPTION
#


