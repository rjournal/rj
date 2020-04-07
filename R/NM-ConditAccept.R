
require(gitR)  # on NM repo

######################  conditional accept  ##################################

# cond. accept  manuscript number msNum: edit DESCRIPTION

# run from Submissions/

conditAccNM <- function(msNum,attaches) {
   print('"attaches" is with respect to correspondence/')
   # get in-memory DESCRIPTION file
   if (!exists('desFiles'))  {
      getAll()
      print('desFiles regenerated')
   }
   des <- desFiles[[msNum]]
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no) ')
   if (ans != 'yes') stop('wrong file')
   # add 'condit. accept' line
   condAccLine <- paste0('  ',Sys.Date(),' condit. accept')
   cmt <- readline('optional brief comment (if none, hit Enter): ')
   if (length(cmt) > 0)
      condAccLine <- paste0(condAccLine,' [',cmt,']')
   des <- c(des,condAccLine) 
   desFiles[[msNum]] <<- des
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
   gitPush()
   autinfo <- getAutInfo(des)
   setwd(paste0(msNum,'/correspondence'))
   sendLetter(msNum,autinfo[1],autinfo[2],'"your R Journal submission"',
      'ConditAccTmpltNM.R',attaches)
}

