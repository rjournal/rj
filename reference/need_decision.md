# Find articles that have at least two completed reviews and need a decision

Returns all articles with at least 2 completed reviews but no decision
This should be run regularly and decisions made if necessary.

## Usage

``` r
need_decision(editor)
```

## Arguments

- editor:

  The editor or associate editor handling the submissions

## Value

A data frame of papers needing more reviewers
