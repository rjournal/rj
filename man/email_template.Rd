\name{email_template}
\alias{email_template}
\title{Send an email template.}
\usage{
email_template(article, template)
}
\arguments{
  \item{article}{An article id, e.g. \code{"2013-01"}}

  \item{template}{The name of a template (without
  extension) found in \code{inst/templates}.}
}
\description{
Interpolate article values into a template, and create a
new (unsent) email in your default mail client.  The email
is created using \code{\link{browseURL}} and the mailto
protocol, so it must be relatively brief.
}
\section{Text format}{
  The template should be divided into header and body with
  \code{---}. The header should contain fields and values
  separated by \code{:} - only a limited
}

\section{Template parameters}{
  The templates use whisker to insert template values.
  These have the form \code{\{\{field_name\}\}}. You can
  use any field from the description as well as the
  following special fields:

  \itemize{ \item name: the name of the first author \item
  email: email address of first author \item editor: name
  of editor \item me: your name, as determine by envvar
  \code{RJ_NAME} }
}

