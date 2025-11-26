# Add the review file received from the reviewer

This function adds the review file received from the reviewer to the
correspondence folder of the article.

## Usage

``` r
add_review(
  article,
  reviewer_id,
  review,
  recommend = NULL,
  date = Sys.Date(),
  AE = is_AE()
)
```

## Arguments

- article:

  Article id, like `"2014-01"`

- reviewer_id:

  Numeric, the index of the intended reviewer in the Reviewer field. 1
  for the first reviewer, 2 for the second

- review:

  Path to the review file, e.g. pdf, txt, or docx format. If not
  specified it is assumed that you added the new file into the
  correspondence directory and the last file for that reviewer will be
  used. If you specify `<i>-review-<j>.` filename (no path) and it
  already exists in the correspondence directory, it will be used.

- recommend:

  Reviewer's recommendation, one of: Accept, Minor, Major, Reject. If
  not specified, an attempt is made to auto-detect it from the file by
  looking at the first occurrence of those keywords. If auto-detect
  fails, use "Received".

- date:

  Date of the comment, defaults to today's date

- AE:

  Logical, if `TRUE` then `"AE: "` prefix is added to the
  recommendation.

## Examples

``` r
if (FALSE) { # \dontrun{
# add review file from the first reviewer and recommend accepting it
add_review("2020-114", reviewer_id = 1, review = file.choose(), recommend = "Accept")
} # }
```
