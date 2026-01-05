# Create a S3 status object

Create a S3 status object

## Usage

``` r
status(status, date = Sys.Date(), comments = "")
```

## Arguments

- status:

  A string description the status. Must be listed in
  [`valid_status`](https://rjournal.github.io/rj/reference/valid_status.md)

- date:

  Date, defaults to today. Must be after 2002-01-01 and not in the
  future.

- comments:

  any additional extra comments

## Examples

``` r
status("rejected")
#> 2026-01-05 rejected 
c(status("rejected"), status("accepted"))
#> 2026-01-05 rejected
#> 2026-01-05 accepted
```
