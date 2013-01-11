# RJ

## Overall goals

* A minimal set of functions to automate manual tasks

* Index.dcf still maintainable by hand, if desired

## Index.dcf format

The `index.dcf` is a DCF (debian control file) file, made up of entries of the following form.

    ID: 2012-01
    Title: Exploring the wonderful world of R
    Authors: 
      "John Smith" <john.smith@gmail.com>
    Editor: Hadley Wickham
    Reviewers: 
      "Marleen Harris" <marline.harris@hotmail.com>, 
      "Roger Swansong" <iloveswans52@yahoo.com>
    Status: 
      2012-05-04 Received,
      2012-05-18 Acknowledged,
      2012-09-12 Under review
      2012-10-12 Minor revision,
      2012-11-12 Accepted,
      2012-12-12 Proofed,
      2013-01-01 Published

### Status

Some statuses can be automatically determined:

* If editor field is blank, then status is "Needs editor"
* If reviewers field is blank, then status is "Needs reviewers"

Otherwise we have the following possibilities

* Initial:
  * Received
  * Acknowledged

* In progress:
  * Under review
  * Major revision
  * Minor revision

* Terminal:
  * Rejected
  * Published
  * Withdrawn

## Example code

Need to make it easier to compute on, so we could do (e.g.):

    library(rj)
    find("2010-15")

    accept("2010-15")
    reject("2010-15")
    withdraw("2010-15")
    # all of these would have manual confirmation question

    assign_editor("2010-15", "HW")
    add_reviewers("2010-15")

    review("2010-15", "major") # partially matched to major revision
    review("2010-15", "minor")
    proof("2010-15")

    # These are all implemented using
    add_status("2010-15", "major", today(), "comments")

    next_id()
    # [1] "2012-38"

    status()
    # prints status of all articles, suitable for inclusion in an email
    # broken down by articles and by editors
    email_status()
    # emails R-journal-editors list

    update_author("2012-38")
    # emails author current status of article
    # using browseURL("mailto:...")
    # if no acknowledgement status is present, this would add it.

    validate()
    validate("2012-15")
    validate(find("2012-15")$authors)

## Submissions

Could use `wufoo` form to accept submissions - this would make it easier to collect details to a common standard, and would be possible to automate using wufoo's REST API. 

Would need additional field in local data to record unique wufoo id, used to prevent duplicate submissions.

