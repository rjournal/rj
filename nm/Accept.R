
######################  accept  ##################################

# accepts manuscript number msNum: edit DESCRIPTION; push DESCRIPTION
# and new author files

# run from Submissions/

accept <- function(msNum,newAutFiles='') {
   # get in-memory DESCRIPTION file for this ms
   getAll()
   # double check it's the right one
   cat('\ncheck correct ms!\n')
   des <- desFiles[[msNum]]  
   print(des)
   ans <- readline('correct file (yes or no)? ')
   if (ans != 'yes') stop('wrong file')
   # descend to ms dir
   subsdir <- getwd()
   on.exit(setwd(subsdir))
   setwd(msNum)
   # updated author files pushed to GitHub yet?
   print('the new author files to be pushed to GitHub are:')
   cat(newAutFiles,'\n')
   ans <- readline('correct files (yes or no)? ')
   if (ans != 'yes') stop('wrong file list')
   if (newAutFiles != '')  {
      pushToGitHub(newAutFiles,'"new author files"')
   }
   # add 'accept' line to DESCRIPTION file
   accLine <- paste0('  ',Sys.Date(),' accept')
   # allow for Roger Bivand-style comment
   cmt <- readline('optional brief comment (if none, hit Enter): ')
   if (cmt != '')
      accLine <- paste0(accLine,' [',cmt,']')
   des <- c(des,accLine) 
   desFiles[[msNum]] <<- des  # update in memory
   # save to actual file
   outfilename <- 'DESCRIPTION'
   cat(des,file=outfilename,sep='\n')
   # push DESCRIPTION to GitHub
   pushToGitHub('DESCRIPTION','"accept"')
   # mv to Accepted
   setwd(subsdir)
   pushToGitHub(msNum,'accept','mv','../Accepted')
   sendLetter(msNum,autinfo[1],autinfo[2],'"your R Journal submission"',
      'AcceptTmplt.R')
}

