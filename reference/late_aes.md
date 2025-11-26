# Find late AEs for submissions being handled by a given editor

This should be run regularly and AEs chased up if they are late.

## Usage

``` r
late_aes(editor)
```

## Arguments

- editor:

  The editor handling the submissions

## Value

A data frame of AEs who are late in handling a paper.

## Details

The number of stars has the following meaning:

- 1 star: not responded in more than 12 weeks;

- 2 stars: not responded in more than 18 weeks;

- 3 stars: not responded in more than 24 weeks;

Please chase up late AEs.
