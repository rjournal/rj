% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfc822.R
\name{address}
\alias{address}
\title{An S3 class to represent email addresses.}
\usage{
address(email = NULL, name = NULL, comment = NULL)
}
\arguments{
\item{email}{Email address of the reviewer}

\item{name}{Display name, optional}

\item{comment}{comment, optional}
}
\description{
An S3 class to represent email addresses.
}
\examples{
address("h.wickham@gmail.com")
address("h.wickham@gmail.com", "Hadley Wickham")
}
