# Extract corresponding author from an article

Extract corresponding author from an article

## Usage

``` r
corr_author(article)
```

## Arguments

- article:

  Article id, like `"2014-01"`

## Examples

``` r
if (FALSE) { # \dontrun{
# extract from a single article
corr_author("Submissions/2020-114")

# extract corresponding authors from the active articles
all <- active_articles()
purrr::map_dfr(all, corr_author)
} # }
```
