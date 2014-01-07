# RJ

This package provides tools for dealing with the metadata for the R Journal. The overall goal of the package is to automate common tasks performed by the editors, particularly the editor-in-chief.

## `DESCRIPTION` format

Each submission consists of a directory containing the submission, any correspondence, and a `DESCRIPTION` file.  The `DESCRIPTION` is a DCF (debian control file) file, made up of entries of the following form.

```
Title: Exploring the wonderful world of R
Authors: 
  "John Smith" <john.smith@gmail.com>
Editor: Hadley Wickham
Reviewers: 
  "Marleen Harris" <marline.harris@hotmail.com>, 
  "Roger Swansong" <iloveswans52@yahoo.com>
Status: 
  2012-05-04 submitted,
  2012-05-18 acknowledged [optional comment],
  2012-09-12 out for review,
  2012-10-12 minor revision,
  2012-11-12 accepted,
  2012-12-12 proofed,
  2013-01-01 published
```

Most of these fields are self-explanatory, apart from the `Status` field, which needs a little more detail. Some statuses can be automatically determined:

* If editor field is blank, then status is "Needs editor"
* If reviewers field is blank, then status is "Needs reviewers"

See `valid_status` for a list of all known statuses, and `todo()` for the next action to be taken based on each status.

## Useful actions
  
```R
library(rj)
# Make sure your working directory in the articles directory then:

as.article("2010-15")
new_id()
report()

# Accept, reject or withdraw an article, by updating status, moving to
# correct directory and drafting emails.
accept("2010-15")
reject("2010-15")
withdraw("2010-15")
```