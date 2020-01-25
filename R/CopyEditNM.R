

##########################  copyedit  ##################################

# send final PDF to author for approval/small changes

# run from Accepted/ or articles/

# arguments:
 
#    msNum: manuscript number
#    deadline: submit approval by this date

copyEdit <- function(msNum,deadline) {
   # need to be in Accepted/ but can start in 'articles'
   here <- getLocalDirName()
   if (here != 'Accepted' && here != 'articles') {
      stop("must start in 'articles/' or 'Accepted'")
   }
   if (here == 'articles') setwd('Accepted')
   # leave trail of bread crumbs
   wd <- getwd()
   on.exit('setwd(wd)')
   getDES <- function(dr) {
      desName <- paste0(dr,'/DESCRIPTION')
      readLines(desName)
   }
   des <- getDES(msNum)
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no) ')
   if (ans != 'yes') stop('wrong file')
   autinfo <- getAutInfo(des)
   pdfLoc <- paste0(msNum,'/RJwrapper.pdf')
   sendLetter(msNum,autinfo[1],autinfo[2],'"your R Journal submission"',
      'CopyEditTmpltNM.R',pdfLoc,deadline=deadline,des=des)
}

