# Summarise the reviewer's agree/decline ratio based on past invites

The function pulls out the agree/decline incidence of all the reviewers
based on past invite and calculate the agree percentage of each
reviewer. Use `tabulate_articles` first to tabulate all the articles in
a particular directory and then apply this function.

## Usage

``` r
reviewer_summary(articles, push = FALSE)
```

## Arguments

- articles:

  a tibble summary of articles in the accepted and submissions folder.
  Output of
  [`tabulate_articles()`](https://rjournal.github.io/rj/reference/tabulate_articles.md)

- push:

  whether the reviewer number of review completed by the reviewer should
  be pushed to the reviewer sheet

## Examples

``` r
if (FALSE) { # \dontrun{
articles <- tabulate_articles()
reviewer_summary(articles)
} # }
```
