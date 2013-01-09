# RJ

## Overall goals

* Minimal set of changes to index.dcf that makes it easier to compute with

* Minimal set of functions to automate manual tasks

* Index.dcf still maintainable by hand

## Submissions

Could use `wufoo` form to accept submissions - this would make it easier to collect details to a common standard, and would be possible to automate using wufoo's REST API. 

Would need additional field in local data to record unique wufoo id, used to prevent duplicate submissions.

## Contact details

I suggest we move to the standard email format (http://tools.ietf.org/html/rfc2822#section-3.4, `name-addr`) to describe name and email address: `"Hadley Wickham" <h.wickham@gmail.com>`. This makes it easier to copy and paste into emails, and remedies the normalisation problem with the current system.

This has a few consequences: 

* Create a new field `Reviewers` which replaces `Reviewer1`, `R1email` etc with a comma separated list

* Combine `Author` and `Email` to give a comma separated list of corresponding author(s).

* Replace `Editor` should be replaced with name-email combo, to make it easier to automate.  Alternatively, we could have a separate csv file giving names of editors, their email address and a abbreviate name for them.

These changes will considerably simplify the underlying object model.

Validating each of these fields would involve splitting by commas, and then confirming each element matched the expression `$"[^]+" <[^>]+>^`.

## Status

Some statuses can be automatically determined:

* If editor field is blank, then status is "Needs editor"
* If reviewers field is blank, then status is "Needs reviewers"

Otherwise we have the following possibilities

* Initial:
  * Received
  * Acknolwedged

* In progress:
  * Under review
  * Major revision
  * Minor revision

* Terminal:
  * Rejected
  * Published = Year-Issue
  * Withdrawn

Currently the status is recorded in two places, `Status` and `Comments`.  This is a duplication of data that leads to inconsistencies.  I suggest we combine  single status and comments fields. The status field becomes a comma separated list of dates (Y-M-D) and statuses with optional comments in square brackets.

So this:

    Status: under review
    Comments: Received 2012-05-04. Acknowledged 2012-05-18.
              Needs a bit of editing. R1 received 2012-09-12.

Would be become

    Status: 
      2012-05-04 received,
      2012-05-18 acknowledged,
      2012-09-12 revision recieved [needs a bit of editing]

To valid: split by `,`, match `(.*?) (.*?) (\\[.*?\\])?`. Check first piece is valid date, second piece is known string.

## Example formatting

    Refno: 2012-18
    Authors: Mathieu Colleter, Jerome Guitton and Didier Gascuel
    Email: m.colleter@fisheries.ubc.ca
    Title: An Introduction to the EcoTroph R package: Analyzing aquatic ecosystem trophic network
    Editor: Heather Turner
    Reviewer1: Karline Soetaert
    R1email: Karline.Soetaert@nioz.nl
    Reviewer2: Cameron Ainsworth
    R2email: ainsworth@usf.edu
    Status: under review
    Comments: Received 2012-05-04. Acknowledged 2012-05-18.
              Needs a bit of editing. R1 received 2012-09-12.

Becomes:

    ID: 2012-18
    Author: "Mathieu Colleter" <m.colleter@fisheries.ubc.ca>
    Title: An Introduction to the EcoTroph R package: Analyzing aquatic 
      ecosystem trophic network
    Editor: Heather Turner
    Reviewers: 
      "Karline Soetaert" <Karline.Soetaert@nioz.nl>, 
      "Cameron Ainsworth" <ainsworth@usf.edu>
    Status: 
      2012-05-04 received,
      2012-05-18 acknowledged,
      2012-09-12 revision recieved, Needs a bit of editing

## Example code

Need to make it easier to compute on, so we could do (e.g.):

    library(rj)
    find("2010-15")

    accept("2010-15")
    reject("2010-15")
    withdraw("2010-15")
    # all of these would have manual confirmation question

    assign_editor("2010-15")
    add_reviewers("2010-15")
    process_submission() # uses wufoo api

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
