# Generates a status plot for articles submitted in the last few years.

Generates a status plot for articles submitted in the last few years.

## Usage

``` r
article_status_plot(years = NULL, save = TRUE)
```

## Arguments

- years:

  years considered. A vector of years, or defaults to last four years.

- save:

  Defaults to TRUE. The plot is saved in the
  rjournal.github.io/resources folder.

## Value

a ggplot, one bar per year (taken from article id)

## Examples

``` r
if (FALSE) { # \dontrun{
article_status_plot()
} # }
```
