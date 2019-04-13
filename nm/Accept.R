
######################  accept  ##################################

# manuscript number msNum: edit DESCRIPTION

# run from Submissions/

accept <- function(msNum) {
   # updated author files pushed to GitHub yet?
   readline('push new author files, if any, to GitHub, then hit Enter): ')
   # get in-memory DESCRIPTION file
   if (is.null(desFiles))  {
      getAll()
      print('desFiles regenerated')
   }
   des <- desFiles[[msNum]]
   # double check it's the right one
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no) ')
   if (ans != 'yes') stop('wrong file')
   # add 'accept' line to DESCRIPTION file
   accLine <- paste0('  ',Sys.Date(),' accept')
   cmt <- readline('optional brief comment (if none, hit Enter): ')
   if (length(cmt) > 0)
      accLine <- paste0(accLine,' [',cmt,']')
   des <- c(des,accLine) 
   desFiles[[msNum]] <<- des  # update in memory
   # save to actual file
   outfilename <- paste0(msNum,'/DESCRIPTION')
   cat(des,file=outfilename,sep='\n')
   # push to GitHub
   cmd <- makeSysCmd('git add ',outfilename)
   cmd()
   cmd <- makeSysCmd('git commit -m "condit. accept"')
   cmd()
   # commit may take a while
   readline('hit Enter when ready')
   ghPush()
   autinfo <- getAutInfo(des)
   sendLetter(msNum,autinfo[1],autinfo[2],'"your R Journal submission"',
      'AcceptTmplt.R')
}

