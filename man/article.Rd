\name{article}
\alias{article}
\title{Create a new article (S3 class)}
\usage{
article(..., quiet = FALSE)
}
\arguments{
  \item{...}{Named arguments giving the components of an
  article: id, authors, title, editor, reviewers, status}
}
\description{
This function will always succeed: if the article is not
parseable it will print an error message and return a
unparsed blob. This ensures that information is not lost
even if some articles have parsing errors.
}

