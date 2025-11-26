# Associate editor (AE) functions

Functions to determine if the user is an AE and retrieve relevant AE
information

## Usage

``` r
AEs()

detect_AE(path = ".", require = FALSE)

AE(path = ".")

is_AE(path = ".")
```

## Arguments

- path:

  string, path to the git repository to use for detection.

- require:

  logical, if `TRUE` then failing to detect the AE is considered an
  error

## Value

- `AEs()`: a data frame with all associate editors

- `detect_AE()`: `NULL` if not found or the row from `AEs()`
  corresponding to the AE

- `AE`: `NULL` if not an AE or a row from `AEs()`

- `is_AE`: `TRUE` if the user if an AE or the repository is an AE
  repository, `FALSE` otherwise

## Details

- `AEs()`: read the associate-editors.csv in the rj package as a data
  frame

- `detect_AE()`: determine AE from the remote of the repository in
  `path` or from the git config e-mail.this only work for AE and will
  fail for editors

- `AE()`: returns the corresponding row from `AEs()` if called by an
  associate editor or in an AE repository, otherwise `NULL`. It relies
  on either `RJ_EDITOR` environment variable to contain the name of the
  editor or detection from the git repository pointed to by `path` (see
  `detect_AE`).

- `is_AE()`: determine if the user is an AE or the repository is an AE
  repository
