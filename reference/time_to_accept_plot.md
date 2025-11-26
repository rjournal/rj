# Generates a plot of acceptance times for articles published in the last few years.

Generates a plot of acceptance times for articles published in the last
few years.

## Usage

``` r
time_to_accept_plot(years = NULL, save = TRUE)
```

## Arguments

- years:

  years considered. A vector of years, or defaults to last four years.

- save:

  Defaults to TRUE. The plot is saved in the
  rjournal.github.io/resources folder.

## Value

a ggplot, one boxplot per publication year

## Examples

``` r
if (FALSE) { # \dontrun{
 time_to_accept_plot()
} # }
```
