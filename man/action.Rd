\name{update_status}
\alias{accept}
\alias{reject}
\alias{update_status}
\alias{withdraw}
\title{Update the status of an article.}
\usage{
update_status(article, status, comments = "", date = Sys.Date())

reject(article, comments = "", date = Sys.Date())

accept(article, comments = "", date = Sys.Date())

withdraw(article, comments = "", date = Sys.Date())
}
\arguments{
  \item{article}{as \code{\link{article}} object, path, or
  ID. See \code{\link{as.article}} for more details about
  how article is located.}

  \item{status}{new status to add}

  \item{comment}{any additional comments}

  \item{date}{date of status update. If omitted defaults to
  today.}
}
\description{
\code{reject}, \code{accept} and \code{withdraw} update the
status, move the file to the correct directory and draft an
email from a template of the corresponding name.
}

