
######################  accept  ##################################

# accepts manuscript number msNum: edit DESCRIPTION; push DESCRIPTION
# and new author files

# run from Submissions/

accept <- function(msNum,newAutFiles='') {
   # updated author files pushed to GitHub yet?
   print('the new author files to be pushed to GitHub are:')
   cat(newAutFiles,'\n')
   ans <- readline('correct files (yes or no)? ')
   if (ans != 'yes') stop('wrong file list')
   if (newAutFiles != '') 
      pushToGitHub(newAutFiles,'new author files')
   # get in-memory DESCRIPTION file for this ms
   getAll()
   des <- desFiles[[msNum]]  
   # double check it's the right one
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no)? ')
   if (ans != 'yes') stop('wrong file')
   # add 'accept' line to DESCRIPTION file
   accLine <- paste0('  ',Sys.Date(),' accept')
   # allow for Roger Bivand-style comment
   cmt <- readline('optional brief comment (if none, hit Enter): ')
   if (length(cmt) > 0)
      accLine <- paste0(accLine,' [',cmt,']')
   des <- c(des,accLine) 
   desFiles[[msNum]] <<- des  # update in memory
   # save to actual file
   outfilename <- paste0(msNum,'/DESCRIPTION')
   cat(des,file=outfilename,sep='\n')
   # push to GitHub
   pushToGitHub('DESCRIPTION','"accept"')
   cmd <- makeSysCmd('git commit -m "condit. accept"')
   cmd()
   # commit may take a while
   autinfo <- getAutInfo(des)
   sendLetter(msNum,autinfo[1],autinfo[2],'"your R Journal submission"',
      'AcceptTmplt.R')
}

