# Publish a news article

This function will publish a news article to the \`rjournal.github.io\`
repository.

## Usage

``` r
publish_news(news, volume, issue, home = get_articles_path())
```

## Arguments

- news:

  File path to the directory containing the news article to publish

- volume:

  The volume of the article's issue (typically, year - 2008)

- issue:

  The issue number of the article's issue

- home:

  Location of the articles directory

## See also

\`publish_article\`, \`publish_issue()\`
