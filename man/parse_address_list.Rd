\name{parse_address_list}
\alias{parse_address_list}
\title{Parse a string into a rfc822 address list.}
\usage{
parse_address_list(x)
}
\arguments{
  \item{string}{to parse}
}
\value{
a list of \code{\link{address}}es
}
\description{
EBNF at
\url{http://tools.ietf.org/html/rfc2822#section-3.4}
}
\examples{
parse_address_list("<a@b.com> Alison, <c@d.com> Colin")
}

