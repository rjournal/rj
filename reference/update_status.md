# Update the article status

This is a general function for updating the status field in the
DESCRIPTION.

## Usage

``` r
update_status(
  article,
  status,
  comments = "",
  date = Sys.Date(),
  AE = is_AE(),
  replace = TRUE
)
```

## Arguments

- article:

  Article id, like `"2014-01"`

- status:

  new status to add, see details section for more

- comments:

  Any additional comments

- date:

  Date of status update. If omitted defaults to today.

- AE:

  Logical, if `TRUE`, `"AE: "` is prefixed to the status

- replace:

  logical, if the last status already matches `status` then the status
  is only updated if this flag is set to `TRUE`.

## Details

For AEs, status is prefixed with "AE: " and valid status includes "AE:
major revision", "AE: minor revision", "AE: accept", and "AE: reject".

For Editors, use
[`accept()`](https://rjournal.github.io/rj/reference/action.md),
[`reject()`](https://rjournal.github.io/rj/reference/action.md), and
[`withdraw()`](https://rjournal.github.io/rj/reference/action.md) to
update the status as well as draft an email to the correspondence
author.

Check valid status with `valid_status`.

## Examples

``` r
if (FALSE) { # \dontrun{
update_status("2020-114", status = "AE: major revision")
} # }
```
