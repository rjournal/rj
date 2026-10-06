# Associate Editors' Guide

## Mission

As an Associate Editor (AE), you will receive manuscripts from one of
the four current Editors. For each manuscript, you are responsible for
the following steps.

1.  **Decide whether the paper needs reviews.** If you think the article
    is not of sufficient quality for the R Journal, you can write a
    review yourself and recommend rejection without seeking other
    reviews. You can also ask (via the Editor) for the authors to refine
    the paper further before you send it to reviewers.
2.  **Find reviewers.** If the paper needs expert review, find at least
    two reviewers with expertise in the subject area of the submission
    (see the tips below). Reviewers are given about 1–3 months to review
    a paper.
3.  **Make a recommendation.** Recommend one of the following: reject,
    accept with major revisions, accept with minor revisions, or accept
    as is.
4.  **Summarise and notify.** Write a short summary explaining the
    reasons for your recommendation and notify the handling editor. The
    handling editor makes the final decision on the paper and manages
    all communication with the authors.

Once an Editor assigns you a paper, you are responsible for it until you
hand it back. When you have made your recommendation and are ready to
hand the paper back, update your repository and let the handling editor
know via Slack or email.

The expected workload is 1–2 papers per month. Terms are for three
years, with the option to renew.

## Communication

Each AE has a GitHub repository for handling papers, named in the form
`ae-articles-XX`. When an editor assigns you an article, you will find
it in the `Submissions` folder of your repository.

Editors and AEs communicate via Slack or email, and Slack is also used
for general information about operations. The AE channel is
`associate-editors`, and you are welcome to join the other channels that
cover different aspects of operations, such as `rj-software`, `general`
and `journal-website`.

Meetings of the Editors and AEs are usually held every few months, at a
time set by the Editor-in-Chief.

Email is usually the best way to contact reviewers.

Please do not contact authors directly. The Editor is responsible for
all communication with authors, so you should only be in contact with
reviewers and with the handling editor who assigned you the paper.

## Workflow and operations

### Getting started

Install the `rj` package with:

``` r

remotes::install_github("rjournal/rj")
```

The package is updated regularly, so it is worth re-installing it from
time to time.

### Workflow

All the submissions you are handling are in the `Submissions` folder of
your GitHub repository. Each submission has its own folder, named with
the article ID (e.g., `2024-12`), which contains:

- the article files: `RJwrapper.tex`, `.tex` and `.bib` files, and
  possibly `.R` and `.Rmd` files, data and figures.
- our operational files:
  - `DESCRIPTION`, which records the current state of the article. It is
    plain text, but where possible you should modify it with the `rj`
    functions rather than by hand.
  - the `correspondence` folder, which usually contains
    `motivating-letter.pdf`. The invitations to reviewers are added
    here, and the reviews are stored here once reviewers return them.

### Finding reviewers

There are several ways to find reviewers for a paper.

1.  **Match keywords against the reviewer database.**

    We keep a list of potential reviewers, collected through this [form
    for volunteering to review for the R
    Journal](https://docs.google.com/forms/d/e/1FAIpQLSf8EmpF85ASWqPHXqV0vdQd-GHhNBaAZZEYf4qxO3gTl-eGyA/viewform).
    Please fill in the form yourself too.

    The form feeds a
    [spreadsheet](https://docs.google.com/spreadsheets/d/1stC58tDHHzjhf63f7PhgfiHJTJkorvAQGgzdYL5NTUQ/edit#gid=1594007907)
    that
    [`rj::match_keywords()`](https://rjournal.github.io/rj/reference/match_keywords.md)
    uses to match keywords between articles and reviewers. You need
    access to this sheet to use the function; if you can’t see it, ask
    the editor who assigned you the paper.

2.  **Look for authors of related R packages.** The submission may cite
    similar packages, or you may find some in a [CRAN Task
    View](https://cran.r-project.org/web/views/). Authors of similar
    packages are often the best reviewers, and are usually keen to
    review work related to their own.

3.  **Look for authors of recent related papers.** These may be papers
    cited in the submission or ones you find through Google Scholar.
    Stick to papers published in the last few years.

4.  **Check your shortlist.** Once you have a few candidates, look at
    their websites to see (a) how active they are in the area, (b) how
    senior they are, and (c) whether they appear to use R. The ideal
    reviewer works actively in the area, uses R, and does not have heavy
    management responsibilities. Postdocs and junior faculty often make
    the best reviewers: they have more time than senior academics and
    more experience than PhD students. An expert in the topic who does
    not use R can still give helpful comments on the paper, but is less
    likely to comment usefully on the code.

5.  **Rank your candidates.** You need to invite at least two reviewers,
    and it is best to keep some in reserve in case one declines. Think
    about who is most likely to say yes; for example, people are more
    likely to agree if they know you.

6.  **Aim for a mix of expertise.** Where possible, choose reviewers
    with different strengths; for example, one with expertise in the
    statistical methods and another with experience writing R packages.

7.  **Personalise the invitation.** Before sending the review-request
    emails, add a sentence explaining why you are asking that person.
    For example: “As the author of package X, I’d be interested in your
    thoughts on this submission.” Or: “I’m aware of your JRSSB paper on
    XXX, so I’m keen to hear your thoughts on this submission, which
    takes a different approach to the same problem.” Or: “The authors
    compare their package with the method you developed in XXX, so I’d
    like to know your views.” People are more likely to agree when they
    know why they were asked.

8.  **If you only get one review,** you may need to write a review
    yourself.

### Revisions

When the authors submit a revised version, the handling editor may send
it back to you. The new files replace the old ones in the article
folder, and the previous version is zipped into the `history` folder.
You will usually ask the original reviewers to look at the revision,
since they are best placed to judge whether their comments have been
addressed.

There are two ways to record that you have invited a reviewer again.

1.  **Re-invite the existing reviewer (preferred).** Use
    [`invite_reviewer()`](https://rjournal.github.io/rj/reference/invite_reviewers.md)
    with the reviewer’s existing index and a new `prefix` for the round,
    e.g.

    ``` r

    rj::invite_reviewer("2024-12", reviewer_id = 1, prefix = "2")
    ```

    This adds another `Invited <date>` entry to the reviewer’s comments
    in the `DESCRIPTION`, keeping each reviewer’s full history in one
    place. It also drafts an invitation (`2-invite-1.txt`) from the
    template, but it is often easier to reply to your original email
    instead, so the reviewer has the earlier correspondence to hand.
    Attach the revised paper and the authors’ response to the reviews.

2.  **Add the reviewer again.** If a reviewer’s line in the
    `DESCRIPTION` has become too long to read easily, you can add them
    as a new entry with
    [`add_reviewer()`](https://rjournal.github.io/rj/reference/add_reviewer.md).
    The same person then appears more than once in the reviewer list, so
    make sure you use the new index with
    [`agree_reviewer()`](https://rjournal.github.io/rj/reference/decline_reviewer.md),
    [`decline_reviewer()`](https://rjournal.github.io/rj/reference/decline_reviewer.md)
    and
    [`add_review()`](https://rjournal.github.io/rj/reference/add_review.md)
    in this round.

From there, the process is the same as in the first round: use
[`agree_reviewer()`](https://rjournal.github.io/rj/reference/decline_reviewer.md)
or
[`decline_reviewer()`](https://rjournal.github.io/rj/reference/decline_reviewer.md)
when the reviewer responds, and
[`add_review()`](https://rjournal.github.io/rj/reference/add_review.md)
when the review arrives.
[`add_review()`](https://rjournal.github.io/rj/reference/add_review.md)
numbers review files by round automatically, so reviewer 1’s second
review is saved as `2-review-1`. Then make your recommendation with
[`update_status()`](https://rjournal.github.io/rj/reference/update_status.md)
and notify the handling editor as before.

For minor revisions, you may decide to check the changes yourself rather
than going back to the reviewers.

### Package functions

These are the main functions for AE work:

- [`match_keywords()`](https://rjournal.github.io/rj/reference/match_keywords.md)
  finds potential reviewers whose keywords match the article.
- [`add_reviewer()`](https://rjournal.github.io/rj/reference/add_reviewer.md)
  adds a reviewer to the `DESCRIPTION` and drafts an invitation.
- [`invite_reviewers()`](https://rjournal.github.io/rj/reference/invite_reviewers.md)
  and
  [`invite_reviewer()`](https://rjournal.github.io/rj/reference/invite_reviewers.md)
  draft invitations to all reviewers, or to one.
- [`agree_reviewer()`](https://rjournal.github.io/rj/reference/decline_reviewer.md)
  and
  [`decline_reviewer()`](https://rjournal.github.io/rj/reference/decline_reviewer.md)
  record a reviewer’s response.
- [`late_reviewers()`](https://rjournal.github.io/rj/reference/late_reviewers.md)
  lists reviewers whose reviews are overdue.
- [`add_review()`](https://rjournal.github.io/rj/reference/add_review.md)
  saves a returned review and records the reviewer’s recommendation.
- [`update_status()`](https://rjournal.github.io/rj/reference/update_status.md)
  records your recommendation, using `AE: reject`, `AE: major revision`,
  `AE: minor revision` or `AE: accept`.
- `valid_status` lists all the available statuses.

## Resources

This document is provided as a vignette in the `rj` package.
