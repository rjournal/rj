library(rj)
setwd("/data/ncsg3/github/r-journal//articles/")
summarise_articles("CG")
article = "2020-33"
reviewer_id = 3
from = "~/Downloads/BivRec-review.pdf"
add_review(article, reviewer_id, from, "Reject")

decline_reviewer(article, 1)


ext = tools::file_ext(from)
prefix = length(list.files(dest, pattern = paste0("-review-", reviewer_id))) + 1
name <- paste0(prefix, "-review-", reviewer_id, ".", ext)
path <- file.path(dest, name)
file.copy(from, to = path)


setwd("/data/ncsg3/github/r-journal/rj/")
