
# an article might have more than one submission; this may be due to
# running get_submissions() more than once due to executon errors, or
# to an author submitting an update without EiC's knowledge

# run from Submissions/

checkDupSubmissions <- function() {
      rj:::getAll()
      dirs <- list.dirs(recurs=FALSE)
      titles <- sapply(desFiles,function(dfile) dfile[1])
      byTitle <- split(dirs,titles)
      nSameTitle <-sapply(byTitle,length)
      which(nSameTitle > 1)
}

