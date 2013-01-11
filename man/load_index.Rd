\name{load_index}
\alias{load_index}
\alias{save_index}
\title{Load/save the index.dcf file.}
\usage{
  load_index(path, quiet = FALSE)

  save_index(index, quiet = FALSE)
}
\arguments{
  \item{path}{to file to load}

  \item{quiet}{if \code{TRUE} no output is produced.}

  \item{index}{index file to re-save.}
}
\description{
  An index object saves both the contents and the location
  of the file, so it can easily be resaved.
}
\examples{
index <- load_index(sample())
index
save_index(index)
}

