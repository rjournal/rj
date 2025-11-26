# Download submissions.

Obtains submissions from the Google Sheets spreadsheet and downloads
submission files from Google Drive.

## Usage

``` r
get_submissions(dry_run = FALSE)
```

## Arguments

- dry_run:

  Use TRUE for testing, which will not change the sheet. Default is
  FALSE.

## Process

The function does three things automatically:

1.  Downloads and extracts submissions into appropriate directories.

2.  Marks submissions as "read" in the spreadsheet.

3.  Uploads acknowledgement emails to gmail account as drafts.

The user (editor-in-chief) then:

1.  Ensures that the files have unzipped correctly (some authors
    incorrectly upload .rar or .tar.gz files) and that the latex
    compiles

2.  Manually sends the draft emails
