
require(gitR)  # on NM repo

# largely does the same work as rj::get_submissions(), but for a single
# .zip file, assumed previously extracted from GMail

# actions:

#    determine next available manuscript number
#    mkdir in Submissions/ with that number
#    cd to the new directory
#    unzip articleZip
#    do some checks
#    create DESCRIPTION file
#    push to GitHub

# call from articles/

# work in progress

getSubmission <- function(articleZip) 
{
browser()
   currdir <- getwd()
   on.exit(setwd(currdir))

   tmp <- new_id()
   newID <- paste0(tmp$year,'-',tmp$seq)
   newDir <- paste0('Submissions/',newID)
   dir.create(newDir)
   setwd(newDir)
   unzip(articleZip)
   checkNewSubmit()
}

