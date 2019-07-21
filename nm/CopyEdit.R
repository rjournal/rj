

##########################  copyedit  ##################################

# send final PDF to author for approval/small changes

# run from Accepted/

copyEdit <- function(msNum,pdf,deadline) {
   # get DESCRIPTION file
   wd <- getwd()
   # leave trail of bread crumbs
   on.exit('setwd(wd)')
   # need to be in Accepted/ but can start in 'articles'
   here <- getLocalDirName()
   if (here != 'Accepted' && here != 'articles') {
      stop("must start in 'articles/' or 'Accepted'")
   }
   if (here == 'articles') setwd('Submissions')
   dirs <- list.dirs(full.names=FALSE,recursive=FALSE)
   getDES <- function(dr) {
      desName <- paste0(dr,'/DESCRIPTION')
      readLines(desName)
   }
   desFile <- getDES(msNum)
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no) ')
   if (ans != 'yes') stop('wrong file')
   sendLetter(msNum,autinfo[1],autinfo[2],'"your R Journal submission"',
      'ConditAccTmplt.R',attaches,deadline=deadline)
}

