\name{invite_reviewers}
\alias{invite_reviewer}
\alias{invite_reviewers}
\title{Invite reviewer(s).}
\usage{
invite_reviewers(article, prefix = "1")

invite_reviewer(article, reviewer_id, prefix = "1")
}
\arguments{
  \item{article}{article id, like \code{"2014-01"}}

  \item{reviewer_id}{invite just a single reviewer}

  \item{prefix}{prefix added to start file name - used to
  distinguish between multiple rounds of reviewers (if
  needed)}
}
\description{
Once you have added reviewers to the DESCRIPTION file, you
can use this function to draft invite emails to them all.
As well as drafting the emails, it also caches them locally
in the \code{correspodence/} directory of the corresponding
article.
}

