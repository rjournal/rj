\name{publish}
\alias{publish}
\title{Publish an article to online first}
\usage{
publish(article, home = getwd())
}
\description{
Publish an article to online first
}
\examples{
\dontrun{
arts <- accepted_articles()
for(art in arts) publish(art)
for(art in arts) bundle(art, "~/desktop")
for(art in arts) email_template(art, "copy-edit")
online_metadata()
}
}

