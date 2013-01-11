\name{load_index}
\alias{load_index}
\alias{save_index}
\title{Read/write the index.dcf file.}
\usage{
  load_index(path = "index.dcf", quiet = FALSE)

  save_index(index)
}
\description{
  Read/write the index.dcf file.
}
\examples{
index <- load_index(sample())
index
save_index(index)
}

