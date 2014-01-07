\name{address}
\alias{address}
\alias{parse_address}
\title{An S3 class to represent email addresses.}
\usage{
address(email = NULL, name = NULL)

parse_address(x)
}
\arguments{
  \item{email}{email address}

  \item{name}{display name, optional}
}
\description{
An S3 class to represent email addresses.
}
\examples{
address("h.wickham@gmail.com")
address("h.wickham@gmail.com", "Hadley Wickham")
parse_address("Hadley Wickham <h.wickham@gmail.com>")
parse_address("<h.wickham@gmail.com>")
}

