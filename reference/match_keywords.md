# Find reviewers through keywords matching

Find reviewers for an article through matching the article keywords to
the keywords reviewers provided when registering. Notice that a
googlesheet authenticate, with your email address printed, will first
pop up to verify the access to the reviewer googlesheet.

## Usage

``` r
match_keywords(id, n = 5)
```

## Arguments

- id:

  the article id in the description file

- n:

  numeric; number of reviewer to display

## Details

All the reviewers are ranked based on the number of matching keywords
and when there is a tie, a random draw is used.

For example, an article A has 3 keywords. Two reviewers have all the 3
keywords matched, 5 reviewers have 2 matches, and another 10 have 1
match. To get 5 reviewers for article A, both reviewers with 3 matches
are in and a random draw, among the five reviewers with 2 matches, is
used to fill the remaining 3 places.

## Examples

``` r
if (FALSE) { # \dontrun{
m1 <- match_keywords("2021-13")
m2 <- match_keywords("2021-13", n = 10)
} # }
```
