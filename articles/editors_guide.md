# Editors' Guide

## Mission

Each Executive Editor (EE) of the R Journal is the primary article
handler for about a third of submissions per year, responsible for:

- matching the article with an appropriate Associate Editor (AE) or
  handling obtaining reviews without an AE.
- communicating with AEs on submitted articles, and if necessary
  providing guidance to an AE about use of GitHub, and relevant `rj`
  functionality.
- communicating progress and reviews with the corresponding author.
- making a final decision if an article should be accepted, or rejected,
  or on an appropriate revision status, usually based on reviewer and AE
  feedback.
- maintaining and enhancing the `rj` and `rjtools` software, in
  collaboration with other EEs and AEs.

## Getting started

Install the `rj` package with

``` r
remotes::install_github("rjournal/rj")
```

The package is being updated and revised regularly, so you should
re-install occasionally.

## Emails using the `rj` package

All communications with authors should be generated using the `rj`
package to ensure correspondence is recorded in the GitHub `articles`
repo.

To ensure the email functions work correctly, please add the following
line to your `.Rprofile` file.

    options(browser=Sys.getenv('R_BROWSER'))

## Assignments

The EIC will assign submissions to you. Notification is via an email to
the author, cc’d to you.

With each submission, you can decide to:

- [`reject()`](https://rjournal.github.io/rj/reference/action.md) the
  paper as unsuitable for the R Journal.
- Choose to send it to an AE via
  [`add_ae()`](https://rjournal.github.io/rj/reference/add_ae.md), or
  handle it yourself.
- If you handle it yourself, please find suitable reviewers as per the
  [Associate Editor
  Guide](https://rjournal.github.io/rj/articles/associate_editors_guide.md).

Once you have received reports from reviewers or the AE, you can decide
to:

- [`reject()`](https://rjournal.github.io/rj/reference/action.md) the
  paper
- Request a
  [`major_revision()`](https://rjournal.github.io/rj/reference/action.md)
- Request a
  [`minor_revision()`](https://rjournal.github.io/rj/reference/action.md)
- [`accept()`](https://rjournal.github.io/rj/reference/action.md) the
  paper

## Article assignment to AE

The keywords of a paper should be matched against the keywords of AEs
available in `associate-editors.csv`. Also, the function
[`ae_workload()`](https://rjournal.github.io/rj/reference/ae_workload.md)
should be used to ensure that the potential AE hasn’t got too many
current assignments.

A newly assigned article for an AE needs to have the directory copied
from the `articles` repo to the relevant `ae-articles-XX`. The
`articles` repo is the master copy, and once a paper is retrieved from
an AE, the `DESCRIPTION` file is updated, and the `correspondence`
folder is populated.

Notify the AE of the assignment. Make sure the AE responds so you can be
sure they are handling the submission.

Chase up AEs who have been handling a submission for more than 3 months
to check progress.

## Checking progress

You should check progress of papers you are handling at least weekly.

- `report(editor = "XX")` where `XX` is your initials provides a
  convenient summary of papers allocated to you. Submissions with three
  stars need urgent attention. Submissions with two stars may also need
  attention.
- `summarise_articles(editor = "XX")` provides a more detailed summary
  of each paper.
- `late_aes(editor = "XX")` gives a list of AEs who have not submitted
  their reviews on time.
- `late_reviewers(editor = "XX")` gives a list of reviewers who have not
  responded or submitted their reviews on time.

## Communications

Slack or email is used for communication, between EIC, EEs and AEs, and
general information about operations. The channel `editors_private` can
be used for protected conversations between EEs.

Monthly virtual meetings are held to keep communication between editors
current.

Email is the primary manner for communicating with authors. The EE, not
AEs, should communicate with authors.

## Timeline for reviews

This is what the acknowledgement letter tells authors about the timeline
for their paper to be handled:

> The submission process proceeds as follows:
>
> - review by editorial board & assignment of associate editor (~2-3
>   weeks)
> - reviewers solicited (~2-4 weeks)
> - reviews received (~2-3 months)

## Accepted articles

Once accepted, the authors should submit the final version within two
weeks. Once submitted, add this to the Accepted article and mark it as
proofed using `rj::update_status("<article_id>", "proofed")`.

## Dealing with problematic authors

A few problematic authors are listed in `vexing_authors.csv`

Please advise the EIC if you think anyone needs to be added to this
list.
