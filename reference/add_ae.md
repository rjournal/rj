# Add AE to the DESCRIPTION

Fuzzy match to find the initial of the AE to fill in the article
DESCRIPTION. Checks that AE term has not ended. The status field is also
updated with a new line of add AE.

## Usage

``` r
add_ae(article, name, date = Sys.Date())
```

## Arguments

- article:

  article id

- name:

  a name used to match AE, can be AE initials, name, github handle, or
  email

- date:

  the date for updating status
