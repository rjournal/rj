# Update reviewer's response to invite

This function updates the reviewers field in the DESCRIPTION with
reviewer's response: accept, decline, or abandon if no reply from the
reviewer for a period of time.

## Usage

``` r
decline_reviewer(article, reviewer_id)

agree_reviewer(article, reviewer_id)

abandon_reviewer(article, reviewer_id)
```

## Arguments

- article:

  Article id, like `"2014-01"`

- reviewer_id:

  Numeric, the index of the intended reviewer in the Reviewer field. 1
  for the first reviewer, 2 for the second

## Examples

``` r
if (FALSE) { # \dontrun{
# first reviewer declined
decline_reviewer("2020-114", reviewer_id = 1)

# second reviewer agreed
agree_reviewer("2020-114", reviewer_id = 2)

# third reviewer doesn't reply and deemed abandon
abandon_reviewer("2020-114", reviewer_id = 3)
} # }
```
