

######################  reject  ##################################

# reject manuscript number msNum: edit DESCRIPTION; git mv to Rejects/

# run from Submissions/


reject <- function(msNum) {
   # get in-memory DESCRIPTION file
   getAll()
   des <- desFiles[[msNum]]
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no) ')
   if (ans != 'yes') stop('wrong file')
   # add 'rejected' line
   rejectLine <- paste0('  ',Sys.Date(),' rejected')
   cmt <- readline('optional brief comment (if none, hit Enter): ')
   if (length(cmt) > 0)
      rejectLine <- paste0(rejectLine,' [',cmt,']')
   des <- c(des,rejectLine) 
   desFiles[[msNum]] <<- des
   # save to actual file
   outfilename <- paste0(msNum,'/DESCRIPTION')
   cat(des,file=outfilename,sep='\n')
   # push to GitHub
   cmd <- makeSysCmd('git add ',outfilename)
   cmd()
   cmd <- makeSysCmd('git commit -m "rejected"')
   cmd()
   # commit may take a while
   readline('hit Enter when ready')
   ghPush()
   cmd <- makeSysCmd('git mv ',msNum,' ../Rejected')
   cmd()
   readline('hit Enter when ready')
   cmd <- makeSysCmd('git commit -m "rejected"')
   cmd()
   # commit may take a while
   readline('hit Enter when ready')
   ghPush()
   print('NOTE: send letter to authors')
}

