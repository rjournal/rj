# RJ

This package provides tools for dealing with the metadata for the R Journal.

## Overall goals

* A minimal set of functions to automate manual tasks

* `DESCRIPTION` files still maintainable by hand

## `DESCRIPTION` format

Each submission consists of a directory containing the submission, any correspondence, and a `DESCRIPTION` file.  The `DESCRIPTION` is a DCF (debian control file) file, made up of entries of the following form.

    Title: Exploring the wonderful world of R
    Authors: 
      "John Smith" <john.smith@gmail.com>
    Editor: Hadley Wickham
    Reviewers: 
      "Marleen Harris" <marline.harris@hotmail.com>, 
      "Roger Swansong" <iloveswans52@yahoo.com>
    Status: 
      2012-05-04 received,
      2012-05-18 acknowledged [optional comment],
      2012-09-12 out for review
      2012-10-12 minor revision,
      2012-11-12 accepted,
      2012-12-12 proofed,
      2013-01-01 published

Most of these fields are self-explanatory, apart from the `Status` field, which needs a little more detail. Some statuses can be automatically determined:

* If editor field is blank, then status is "Needs editor"
* If reviewers field is blank, then status is "Needs reviewers"

Otherwise we have the following possibilities

* Initial:
  * received
  * acknowledged

* In progress:
  * out for review
  * major revision
  * minor revision
  * updated
  * proofed
  * online

* Terminal:
  * rejected
  * published
  * withdrawn

## Examples
  
    library(rj)
    # Make sure your working directory is set correct, then:

    as.article("2010-15")
    new_id()
    report()

    # These are easy to do by hand, but this will set the date 
    # correctly and move to the correct location
    accept("2010-15")
    reject("2010-15")
    withdraw("2010-15")

## Submissions

Could use `wufoo` form to accept submissions - this would make it easier to collect details to a common standard, and would be possible to automate using wufoo's REST API. 

Would need additional field in local data to record unique wufoo id, used to prevent duplicate submissions.

