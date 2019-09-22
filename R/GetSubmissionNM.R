
require(gitR)  # on NM repo

# largely does the same work as rj::get_submissions()

# actions:

#  determine next available manuscript number
#  mkdir in Submissions/ with that number
#  cd to the new directory
#  unzip articleZip
#  do some checks
#  create DESCRIPTIONfile
#  push to GitHub

# call from articles/

# work in progress

getSubmission <- function(articleZip) 
{
   currdir <- getwd()
   on.exit(setwd(currdir))

   newID <- new_id()
   newDir <- paste0('Submissions/',newID)
   dir.create(newDir)
   setwd(newDir)
   unzip('articleZip)

}

# newSubmission <- function() {
#    require(gitR)
#    oldSubs <- dir('Submissions')
#    # can't have any files other that article directories
#    ds <- list.dirs('Submissions',recursive=FALSE)
#    if (length(oldSubs) > length(ds))
#       stop('remove non-article files/directories first')
#    # rj:::get_submissions()
#    currentSubs <- dir('Submissions')
#    newSubs <- setdiff(currentSubs,oldSubs)
#    if (currentSubs == newSubs) load('newSubs')
#    cat('new submissions:\n',newSubs)
#    save(newSubs,file='newSubs')
#    ACK(newSubs)
#    gitOpPush(newSubs,'"new submissions"')
# }

