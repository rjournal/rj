# Functions for proofing articles

Functions for proofing articles

## Usage

``` r
get_accepted_articles()

draft_proofing(article, update = TRUE)

proofing_article(drafts)

proofing_article_text(article)
```

## Arguments

- article:

  article id

- update:

  logical, if `TRUE` then the status is updated to "out for proofing"

- drafts:

  list of `gmail_draft` objects

## Details

- `get_accepted_articles()`: get list of articles in the Accepted folder
  to be proofed. This can be used with `draft_proofing` to construct
  emails to authors on the final version.

- `draft_proofing()`: generate proofing email for one article

- `proofing_article()`: send proofing article emails

- `proofing_article_text()`: writes the email text into the
  correspondence folder
