% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/email.R
\name{email_text}
\alias{email_text}
\title{Generate an email template.}
\usage{
email_text(text, means = getOption("RJ_EMAIL_TOOL", "mailto"))
}
\arguments{
\item{text}{character vector, text of the e-mail (including headers)}

\item{means}{string, one of "mailto", "browser" (both open mailto: URL),
"show" (file.show), "edit" (file.edit), "open" (shell open) or
"clip" (system clipboard). Defaults to RJ_EMAIL_TOOL environment
variable.}
}
\description{
Generate an email template.
}
