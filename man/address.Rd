\name{address}
\alias{address}
\title{An S3 class to represent email addresses.}
\usage{
address(email = NULL, name = NULL)
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
}

