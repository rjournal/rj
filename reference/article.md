# S3 class for article objects

Create or convert input into an s3 article object

## Usage

``` r
article(..., quiet = FALSE)

as.article(id)
```

## Arguments

- ...:

  Named arguments giving the components of an article: id, authors,
  title, editor, reviewers, status

- quiet:

  if `TRUE` suppresses failure messages.

- id:

  a path to a DESCRIPTION, a path to a directory containing DESCRIPTION,
  or a article name, found in a sub-directory rejected, accepted or
  submissions

## Details

if the article is not parsable, `article()` will print an error message
and return a unparsed blob. This ensures that information is not lost
even if some articles have parsing errors.

Usually the best way to use `as_article()` is to have your working
directory set to the admin repo, and refer to articles by their id. See
the examples section.

## Examples

``` r
if (FALSE) { # \dontrun{
as.article("2012-01")
as.article("2012-02")
} # }
```
