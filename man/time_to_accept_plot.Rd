% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_plots.R
\name{time_to_accept_plot}
\alias{time_to_accept_plot}
\title{Generates a plot of acceptance times for articles published in the last few years.}
\usage{
time_to_accept_plot(years = NULL, save = TRUE)
}
\arguments{
\item{years}{years considered. A vector of years, or defaults to last four years.}

\item{save}{Defaults to TRUE. The plot is saved in the rjournal.github.io/resources folder.}
}
\value{
a ggplot, one boxplot per publication year
}
\description{
Generates a plot of acceptance times for articles published in the last few years.
}
\examples{
\dontrun{
 time_to_accept_plot()
}

}
