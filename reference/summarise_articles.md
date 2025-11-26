# Summarise editors current in-tray

This function summarises and prints the articles an editor currently
have in hand. It also prints out the articles that has not been assigned
to any editor on the top, if any. If assigned to an object, the
unassigned articles will appear on the top of the data frame.

## Usage

``` r
summarise_articles(editor = NULL, rejected = FALSE, other = FALSE)

get_assignments(editor, folder = "Submissions")

get_unassigned()

find_articles(editor, folder, role)

get_latest(assignments)
```

## Arguments

- editor:

  Editors initials. If `NULL`, looks for the environment variable
  `RJ_EDITOR`.

- rejected:

  Default `FALSE`. If `TRUE`, show rejected articles.

- other:

  Default `FALSE`. If `TRUE`, list all articles not covered by any of
  the other options (typically accepted and online)

- folder:

  the folder to search for assignments, one of Submissions, Rejected,
  Accepted, or Proofs

- role:

  string, take value of either "Editor" or "AE"

- assignments:

  an output object from `get_assignments()` or `get_unassigned()`
