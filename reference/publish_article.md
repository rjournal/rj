# Publish an individual article

This function will publish an individual article to the
\`rjournal.github.io\` website repo.

## Usage

``` r
publish_article(
  article,
  volume,
  issue,
  home = get_articles_path(),
  web_path = file.path(home, "..", "rjournal.github.io"),
  legacy = FALSE,
  slug
)
```

## Arguments

- article:

  article id

- volume:

  The volume of the article's issue (typically, year - 2008)

- issue:

  The issue number of the article's issue

- home:

  Location of the articles directory

- web_path:

  Location of the web source root of the journal, i.e., where `_article`
  lives. The default assumes all repos are checked out by their name in
  the same top-level directory.

- legacy:

  (Very) old way of referencing the R journal

- slug:

  optional, explicitly set the slug name (for expert use only, useful to
  skip problematic articles by advancing the slug names manually)

## Details

The function will complete the following tasks: 1. Assign an appropriate
slug if one is not set in the article DESCRIPTION 2. Produce a zip
containing supplementary files described in the DESCRIPTION 3. If legacy
PDF article, the articles will be converted into HTML format suitable
for the distill HTML website. If an Rmd file with the output format
\`"rjtools::rjournal_web_article"\` is found, it will be directly copied
across as-is. 4. Set the issue metadata for these articles in the
produced/copied Rmd front matter. 5. Update the status of the article's
DESCRIPTION to 'online' 6. Render the document to update the article's
HTML and PDF output.

## See also

\`publish_issue()\`, \`publish_news()\`
