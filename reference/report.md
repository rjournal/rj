# Generate a status report.

This should be run weekly.

## Usage

``` r
report(articles = active_articles(), editor = NULL)
```

## Arguments

- articles:

  list of articles to generate report for. Defaults to all active
  reports in `Submissions/`.

- editor:

  editor to generate report for. Defaults to all editors.
