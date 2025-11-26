# Invite reviewer(s).

Once you have added reviewers to the DESCRIPTION file, you can use this
function to draft invite emails to them all. As well as drafting the
emails, it also caches them locally in the `correspodence/` directory of
the corresponding article.

## Usage

``` r
invite_reviewers(article, prefix = "1")

invite_reviewer(article, reviewer_id, prefix = "1")
```

## Arguments

- article:

  Article id, like `"2014-01"`

- prefix:

  Prefix added to start file name - used to distinguish between multiple
  rounds of reviewers (if needed)

- reviewer_id:

  Numeric, the index of the intended reviewer in the Reviewer field. 1
  for the first reviewer, 2 for the second
