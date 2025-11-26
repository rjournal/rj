# An S3 class to represent email addresses.

An S3 class to represent email addresses.

## Usage

``` r
address(email = NULL, name = NULL, comment = NULL)
```

## Arguments

- email:

  Email address of the reviewer

- name:

  Display name, optional

- comment:

  comment, optional

## Examples

``` r
address("h.wickham@gmail.com")
#> <h.wickham@gmail.com> 
address("h.wickham@gmail.com", "Hadley Wickham")
#> "Hadley Wickham" <h.wickham@gmail.com> 
```
