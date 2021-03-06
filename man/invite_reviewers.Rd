% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reviewers.R
\name{invite_reviewers}
\alias{invite_reviewers}
\alias{invite_reviewer}
\alias{add_review}
\title{Invite reviewer(s).}
\usage{
invite_reviewers(article, prefix = "1")

invite_reviewer(article, reviewer_id, prefix = "1")

add_review(
  article,
  reviewer_id,
  review,
  recommend = NULL,
  date = Sys.Date(),
  AE = is_AE()
)
}
\arguments{
\item{article}{article id, like \code{"2014-01"}}

\item{prefix}{prefix added to start file name - used to distinguish
between multiple rounds of reviewers (if needed)}

\item{reviewer_id}{invite just a single reviewer}

\item{review}{Path to the review file, e.g. pdf, txt, or docx format. If not specified it is assumed that you added the new file into the correspondence directory and the last file for that reviewer will be used. If you specify \code{<i>-review-<j>.} filename (no path) and it already exists in the correspondence directory, it will be used.}

\item{recommend}{Summary of review, one of: Accept, Minor, Major, Reject. If not specified, an attempt is made to auto-detect it from the file by looking at the first occurrence of those keywords.}

\item{date}{Date of the comment, defaults to today's date}

\item{AE}{logical, if \code{TRUE} then \code{"AE: "} prefix is added to the recommendation.}
}
\description{
Once you have added reviewers to the DESCRIPTION file, you can use
this function to draft invite emails to them all. As well as drafting
the emails, it also caches them locally in the \code{correspodence/}
directory of the corresponding article.
}
