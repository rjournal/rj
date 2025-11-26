# Find late reviewers for submissions being handled by a given editor

This should be run regularly and reviewers chased up if they are late.

## Usage

``` r
late_reviewers(editor)
```

## Arguments

- editor:

  The editor or associate editor handling the submissions

## Value

A data frame of reviewers who have not responded or not submitted their
review on time.

## Details

The number of stars has the following meaning:

- 1 star: not responded in more than a week or accepted more than 4
  weeks ago;

- 2 stars: not responded in more than 2 weeks or accepted more than 8
  weeks ago;

- 3 stars: not responded in more than 3 weeks or accepted more than 12
  weeks ago;

Please chase up late reviewers. If a reviewer has declined, use
[`decline_reviewer`](https://rjournal.github.io/rj/reference/decline_reviewer.md)
to remove them from the list. If you have decided to abandon a reviewer,
use
[`abandon_reviewer`](https://rjournal.github.io/rj/reference/decline_reviewer.md).
