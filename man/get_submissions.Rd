\name{get_submissions}
\alias{get_submissions}
\title{Download submissions from wufoo.}
\usage{
get_submissions()
}
\description{
Requires the wufoo api key to be set in envvar
\code{WUFOO_API_KEY}.
}
\section{Process}{
  \enumerate{ \item Submissions are downloaded and placed
  in new directories in submissions.  \item In wufoo,
  manually mark each new submission as processed (so they
  won't get downloaded again) \item Ensure that the files
  have unzipped correctly (some authors incorrectly upload
  .rar or .tar.gz files) and that the latex compiles \item
  Send off the draft emails that should have accumulated in
  you mail client. }
}

