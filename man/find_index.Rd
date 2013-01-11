\name{find_index}
\alias{as.index}
\alias{find_index}
\title{Find the index file.}
\usage{
  find_index(path = NULL)

  as.index(index)
}
\value{
  For \code{find_index} a path, for \code{as.index} the
  parsed index file.
}
\description{
  Looks first in \code{path} and then in the
  \code{index.dcf} option.
}

