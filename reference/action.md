# Accept, reject, or withdraw an article

This set of functions wraps around
[`update_status()`](https://rjournal.github.io/rj/reference/update_status.md)
and `email_template` to first update the status field in the DESCRIPTION
file and then draft an email from the template. Articles are verified to
be under the Submission folder before carrying out the actions to avoid
mistake input of article ID.

## Usage

``` r
reject(article, comments = "", date = Sys.Date())

reject_format(article, comments = "", date = Sys.Date())

accept(article, comments = "", date = Sys.Date())

withdraw(article, comments = "", date = Sys.Date())

resubmission(article, comments = "", date = Sys.Date())

major_revision(article, comments = "", date = Sys.Date())

minor_revision(article, comments = "", date = Sys.Date())
```

## Arguments

- article:

  Article id, like `"2014-01"`

- comments:

  Any additional comments

- date:

  Date of status update. If omitted defaults to today.
