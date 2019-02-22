
# utility routines to manage the Submissions directory

# launched from that directory, or use gotoSubs() to go there

library(stringr)

# globals

subs <- NULL  # submissions data frame, created by getAll()
desFiles <- NULL # R list, one DESCRIPTION file per element
myRecs <- '~/Minority/General.Notes'  # name of my own records file, if any

######################  getAll  ##################################

# creates data frame on all submissions, one row for each submission; col
# names are Title, Aut, Editor, HasReviewer, Status; this data frame is
# global, named 'subs'

# also creates R list, the global 'desFiles', one DESCRIPTION file per
# element; note: deletes any blank lines at the end of such a file, and
# makes sure a comma is at the end of the last line

getAll <- function() {
   # are we in Submissions/?
   wd <- getwd()
   nc <- nchar(wd)
   dirname = substr(wd,nc-10,nc)
   if (dirname != 'Submissions') stop('must be in Submissions/ dir')
   dirs <- list.dirs(full.names=FALSE,recursive=FALSE)
   getDES <- function(dr) {
      desName <- paste0(dr,'/DESCRIPTION')
      readLines(desName)
   }
   desFiles <<- lapply(dirs,getDES)
   des.files <- lapply(desFiles,fixDesEnd)
   names(des.files) <- dirs
   desFiles <<- des.files
   Titles <- sapply(desFiles,getTitle)
   df <- data.frame(Title=Titles)
   row.names(df) <- dirs
   auts <- sapply(desFiles,getAut)
   df <- cbind(df,Aut=auts)
   eds <- sapply(desFiles,getEd)
   df <- cbind(df,Editor=eds)
   rs <- sapply(desFiles,getReviewStatus)
   subs <<- cbind(df,HasReviewer=rs[1,],Status=rs[2,])
}

######################  fixDesEnd  ##################################

# deletes any blank lines at the end of DESCRIPTION file, and
# and makes sure a comma is at the end of the last line
fixDesEnd <- function(des) {
   # check for Status line
   slines <- grep('Status',des)
   if (length(slines) == 0) {
      print(des)
      stop('no status line')
   }
   statusLineNum <- slines[1]
   # check for blank/empty lines at the end
   # find first empty/nonblank status line
   statusLines <- des[statusLineNum:length(des)]
   firstBlankOrEmpty <- -1
   for (i in 1:(length(statusLines)-1)) {
      if (isBlankOrEmpty(statusLines[i+1])) {
         firstBlankOrEmpty <- statusLineNum + i 
         break
      }
   }
   if (firstBlankOrEmpty != -1)  {  # some blank/empty lines
      des <- des[1:(firstBlankOrEmpty-1)]
   }
   # make sure comma at end of last line
   lastLine <- des[length(des)]
   endCharNum <- nchar(lastLine)
   if (substr(lastLine,endCharNum,endCharNum) != ',') {
      lastLine <- paste0(lastLine,',')
      des[length(des)] <- lastLine
   }
   des
}

isBlankOrEmpty <- function(line) {
   if (nchar(line) == 0) return(TRUE)
   for (i in 1:nchar(line)) 
      if (substr(line,i,i) != ' ') return(FALSE)
   return(TRUE)
}


# get title of paper, truncated to 20 chars
getTitle <- function(des) {
   tlines <- grep('Title',des)
   if (length(tlines) == 0) {
      print(des)
      stop('no title line')
   }
   tline <- des[tlines[1]]
   substr(tline,8,min(27,nchar(tline)))
}

# get lead author's e-mail address
getAut <- function(des) {
   alines <- grep('Authors',des)
   if (length(alines) == 0) {
      print(des)
      stop('no authors line')
   }
   aline <- des[alines[1]+1]
   left <- str_locate(aline,'<')
   right <- str_locate(aline,'>')
   if (length(left) + length(right) == 0) {
      print(des)
      stop('bad author e-mail address')
   }
   emailAddr <- substr(aline,left[1]+1,right[1]-1)
}

# get editor
getEd <- function(des) {
   elines <- grep('Editor',des)
   if (length(elines) == 0) {
      print(des)
      stop('no editor line')
   }
   eline <- des[elines[1]]
   substr(eline,9,nchar(eline))
}

# set editor, directly writing to DESCRIPTION file, and pushing to # GitHub
setEd <- function(msNumList,editor) {
   subsdir <- getwd()
   on.exit(setwd(subsdir))
   for (msNum in msNumList) {
      setwd(msNum)
      des <- readLines('DESCRIPTION')
      elines <- grep('Editor',des)
      print(des[elines[1]])
      toWrite <- paste0('Editor: ',editor)
      print(toWrite)
      ans <- readline('OK to write new entry? ')
      if (ans != 'yes') stop('exiting')
      des[elines[1]] <- toWrite
      writeLines(des,'DESCRIPTION')
      cmd <- makeSysCmd('git add ','DESCRIPTION')
      cmd()
      cmd <- makeSysCmd('git commit -m "set editor"')
      cmd()
      # commit may take a while
      readline('hit Enter when ready')
      ghPush()
      setwd(subsdir)
   }
}

# check has reviewers and status
getReviewStatus <- function(des) {
   rlines <- grep('Reviewers',des)
   if (length(rlines) == 0) {
      print(des)
      stop('no reviewers line')
   }
   rline <- des[rlines[1]]
   left <- str_locate(rline,'<')
   HasReviewer <- !is.na(left[1])
   slines <- grep('Status',des)
   statusLineNum <- slines[1]
   status <- des[length(des)]
   nChars <- min(25,nchar(status))
   status <- substr(status,1,nChars)
   c(HasReviewer,status)
}

######################  autNum  ################################

# inputs author's e-mail address (can be partial), outputs submission
# number, 20XX-Y; assumes 'subs', output of getAll(), is global; numOnly
# means return the submission number only, else the entire line in subs

autNum <- function(aut,numOnly=TRUE) {
   aut <- grep(aut,subs$Aut)
   if (length(aut) > 1) {
      print('multiple author entries')
      print(subs[aut,])
   } else if (length(aut) == 0)
      stop('missing author entry')
   if (numOnly) return(row.names(subs)[aut])
   subs[aut,]
}

######################  pullRepo  ##################################

# update articles/ and re-run getAll()

pullRepo <- function() {
   system('git pull origin')
   subs <<- getAll()
}

######################  gotoSubs  ##################################

# to launch from my Scripts/
gotoSubs <- function() {
   setwd('../articles/Submissions')
}

######################  reject  ##################################

# reject manuscript number msNum: edit DESCRIPTION; git mv to Rejects/

# run from Submissions/


reject <- function(msNum) {
   # get in-memory DESCRIPTION file
   if (is.null(desFiles))  {
      getAll()
      print('desFiles regenerated')
   }
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

###########################  makeSysCmd  ##################################

# utility function to construct a string containing an R command,
# involving system()

# e.g.
#
# g <- makeSysCmd('ls')  # Mac/Linux command to list files
# g()  # is then same as typing system('ls')

makeSysCmd <- function(...) {
   x <- paste(...)
   f <- function() {
       system(x)
   }
   f
}

########################  pushToGitHub  ##################################

# the 'add' argument will be tacked on to 'git add', with the file names
# relative to articles/dir

# example

#   pushToGitHub('xy z','new src files')

# will push xy and z in current directory

pushToGitHub <- function(add,commitComment) {
   cmd <- makeSysCmd('git add ',add)
   cmd()
   cmd <- makeSysCmd('git commit -m ',commitComment)
   cmd()
   # commit may take a while
   readline('hit Enter when ready')
   ghPush()
}


###########################  ghPush  ##################################

# push to GitHub, final action; make it a loop in case of password
# mistyping :-)

ghPush <- function() {
   cmd <- makeSysCmd('git push origin')
   while (TRUE) {
      if (cmd() == 0) return()
   }
}

###########################  editPush  ##################################

# edit file, then push

editPush <- function(fname,commitComment,textEditor='vim') {
   cmd <- makeSysCmd(textEditor,fname)
   cmd()
   pushToGitHub(fname,commitComment)
}
