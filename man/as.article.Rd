\name{as.article}
\alias{as.article}
\title{Convert input into an article.}
\usage{
  as.article(id)
}
\arguments{
  \item{a}{path to a DESCRIPTION, a path to a directory
  containing DESCRIPTION, or a article name, found in a
  sub-directory rejected, accepted or submissions}
}
\description{
  Convert input into an article.
}
\examples{
# Usually the best way to use is to have your working directory set to the
# admin repo, and refer to articles by their id.
\dontrun{
as.article("2012-01")
as.article("2012-02")
}
}

