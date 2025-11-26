# Check the number of articles an AE is currently working on

This will examine the DESCRIPTION files for articles in the Submissions
folder, check articles that have status "with AE".

## Usage

``` r
ae_workload(articles = NULL, day_back = 365, active_only = FALSE)

get_AE(x)
```

## Arguments

- articles:

  a tibble summary of articles in the accepted and submissions folder.
  Output of
  [`tabulate_articles()`](https://rjournal.github.io/rj/reference/tabulate_articles.md)

- day_back:

  numeric; positive number of day to go back for calculating AE
  workload. Retains any article where any status entry for an article is
  newer than \`day_back\` days ago.

- active_only:

  Toggle to show only active AEs (filtered by end year and comment
  field).

- x:

  a single article, i.e. as.article("Submissions/2020-114")

## Examples

``` r
if (FALSE) { # \dontrun{
ae_workload()
} # }
if (FALSE) { # \dontrun{
art <- as.article("Submissions/2020-114")
get_AE(art)
} # }
```
