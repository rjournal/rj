library(rj)
setwd("/data/ncsg3/github/r-journal//articles/")
summarise_articles()
options(browser = "google-chrome")

article = "2020-39"
path = "../articles/Submissions/2020-39/DESCRIPTION"
article = as.article("2020-39")
add_reviewer(article,
             name = "Kris Sankaran",
             email = "kris.sankaran@umontreal.ca",
             invite = TRUE)


file.copy(from = file.path(article$path, "RJwrapper.pdf"),
          to = paste0(file.path("/tmp/",
                                paste(article$id, collapse = "-")
          ), ".pdf"))
setwd("/data/ncsg3/github/r-journal/rj/")
