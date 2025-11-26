# Show articles that require attention with the corresponding action

Show articles that require attention with the corresponding action

## Usage

``` r
actionable_articles(editor, invite = 7, review = 30, verbose = FALSE)
```

## Arguments

- editor:

  string, initial of an editor or an associate editor, defaults to
  `RJ_EDITOR` env var

- invite:

  integer, number of days after which an invite is considered overdue

- review:

  integer, number of days after which a review is considered overdue.
  Note that you can extend this by specifying a due date in the comment,
  e.g.: "2021-03-01 Agreed (until 2021-05-01)" would allow for two
  months.

- verbose:

  logical, if `TRUE` it will always list the full reviewer report for
  each entry
