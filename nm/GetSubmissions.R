
# wraps to rj::get_submissions(), then does a different ACK, with the
# latter calling rj::build_latex() as well

# call from articles/

rewSubmissions <- function() {
   oldSubs <- dir('Submissions')
   # can't have any files other that article directories
   ds <- list.dirs('Submissions,recursive=FALSE)
   if (length(oldSubs) > length(ds))
      stop('remove non-article files/directories first')
   rj:::get_submissions()
   currentSubs <- dir('Submissions')
   newSubs <- setdiff(currentSubs,oldSubs)
   if (currentSubs == newSubs) load('newSubs')
   cat('new submissions:\n',newSubs)
   save(newSubs,file='newSubs')
   ACK(newSubs)
}

