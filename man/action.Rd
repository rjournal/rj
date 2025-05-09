% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/actions.R
\name{reject}
\alias{reject}
\alias{reject_format}
\alias{accept}
\alias{withdraw}
\alias{resubmission}
\alias{major_revision}
\alias{minor_revision}
\title{Accept, reject, or withdraw an article}
\usage{
reject(article, comments = "", date = Sys.Date())

reject_format(article, comments = "", date = Sys.Date())

accept(article, comments = "", date = Sys.Date())

withdraw(article, comments = "", date = Sys.Date())

resubmission(article, comments = "", date = Sys.Date())

major_revision(article, comments = "", date = Sys.Date())

minor_revision(article, comments = "", date = Sys.Date())
}
\arguments{
\item{article}{Article id, like \code{"2014-01"}}

\item{comments}{Any additional comments}

\item{date}{Date of status update. If omitted defaults to today.}
}
\description{
This set of functions wraps around \code{update_status()} and \code{email_template}
to first update the status field in the DESCRIPTION file and
then draft an email from the template. Articles are verified to be under the
Submission folder before carrying out the actions to avoid mistake
input of article ID.
}
