# Find articles that need reviewers for submissions being handled by a given editor.

Returns all articles with fewer than 2 invited reviewers. This should be
run regularly and new reviewers appointed if necessary.

## Usage

``` r
need_reviewers(editor)
```

## Arguments

- editor:

  The editor or associate editor handling the submissions

## Value

A data frame of papers needing more reviewers
