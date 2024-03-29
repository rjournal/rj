% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publish.R
\name{publish_issue}
\alias{publish_issue}
\title{Publish an issue}
\usage{
publish_issue(issue, home = get_articles_path(), republish_all = FALSE)
}
\arguments{
\item{issue}{The name of the issue, for example "2022-1".}

\item{home}{Location of the articles directory}

\item{republish_all}{If TRUE, then all articles and news will be published
again in rjournal.github.io.}
}
\description{
This function will publish an issue in the `rjournal.github.io` repository
from the Proofs folder. If any articles or news from this issue are not yet
published, it will prompt you to also publish these articles and news.
}
\details{
This is the main function to be used for publishing an issue and its contents
to the R Journal website. It completes these steps:

1. Publish any unpublished articles from this issue.
2. Publish any unpublished news from this issue.
3. Generate a templated R Markdown file for the issue, with some metadata
   completed in the document's front matter.
4. Open the generated issue R Markdown file for you to update the metadata,
   for example to update the editors of the issue.
}
