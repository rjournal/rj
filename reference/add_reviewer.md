# Invite an reviewer

This function adds the reviewer information(name and email) to the
reviewers field in the DESCRIPTION as well as draft an email to invite
the reviewer.

## Usage

``` r
add_reviewer(article, name, email, invite = TRUE)
```

## Arguments

- article:

  Article id, like `"2014-01"`

- name:

  Full name of the reviewer

- email:

  Email address of the reviewer

- invite:

  Logical, whether to automatically construct an email to invite the
  reviewer

## Examples

``` r
if (FALSE) { # \dontrun{
add_reviewer("2019-117", "Quiet Quokka", "qqplot@waspot.com")
} # }
```
