
# utility routines to manage the Submissions directory

# launched from that directory, or use gotoSubs() to go there

library(stringr)

# globals

# RJ_NAME environment variable, your name
# RJNM_DIR environment variable, full path of rj/nm
 
# subs <- NULL  # submissions data frame, created by getAll()
# desFiles <- NULL # R list, one DESCRIPTION file per element

######################  getAll  ##################################

# creates data frame on all submissions, one row for each submission; col
# names are Title, Aut, Editor, HasReviewer, Status; this data frame is
# global, named 'subs'

# also creates R list, the global 'desFiles', one DESCRIPTION file per
# element; note: deletes any blank lines at the end of such a file, and
# makes sure a comma is at the end of the last line

getAll <- function() {
   wd <- getwd()  
   # leave trail of bread crumbs
   on.exit('setwd(wd)')
   # need to be in Submissions/ but can start in 'articles'
   here <- getLocalDirName()
   if (here != 'Submissions' && here != 'articles') {
      stop("must start in 'articles/' or 'Submissions'")
   }
   if (here == 'articles') setwd('Submissions')
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
   auts <- sapply(desFiles,getAutAddr)
   df <- cbind(df,Aut=auts)
   eds <- sapply(desFiles,getEd)
   df <- cbind(df,Editor=eds)
   reviewers <- sapply(desFiles,getReviewStatus)
   tmpsubs <- cbind(df,HasReviewer=reviewers[1,],Status=reviewers[2,])
   tmpsubs$HasReviewer <- as.logical(tmpsubs$HasReviewer)
   subs <<- tmpsubs
}

######################  getMSnumByAut  ##################################

# returns the ms number(s) for 'aut', full or unique partial e-mail
# address of the first author

getMSnumByAut <- function(aut) {
   getAll()
   grepout <- grep(aut,subs$Aut)
   if (length(grepout) == 0) {
      stop('author not found')
   }
   rownames(subs)[grepout]
}

######################  getMSnumByTitle  ##################################

# returns the ms number(s) for 'aut', full or unique partial title

getMSnumByTitle <- function(title) {
   getAll()
   grepout <- grep(title,subs$Title)
   if (length(grepout) == 0) {
      stop('title not found')
   }
   rownames(subs)[grepout]
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

# get lead author's and e-mail address
getAutInfo <- function(des) {
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
   tmp <- strsplit(aline,' ')[[1]]
   tmp <- tmp[length(tmp)-1]
   surname <- substr(tmp,1,(nchar(tmp)-1))
   c(surname,emailAddr)
}

getAutAddr <- function(des) {
   getAutInfo(des)[2]
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

# set editor, directly writing to DESCRIPTION file, and pushing to 
# GitHub; msNumEdList is an R list of (nsNum,editor) pairs
setEd <- function(msNumEdList) {
   subsdir <- getwd()
   on.exit(setwd(subsdir))
   for (pair in msNumEdList) {
      msNum <- pair[1]
      editor <- pair[2]
      setwd(msNum)
      des <- readLines('DESCRIPTION')
      elines <- grep('Editor',des)
      print(des[elines[1]])
      toWrite <- paste0('Editor: ',editor)
      print(toWrite)
      ans <- readline('OK to write new entry (yes or no)? ')
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
   rline <- rlines[1]
   leftelbow <- str_locate(rline,'<')
   slines <- grep('Status',des)
   sline <- slines[1]
   HasReviewer <- length(grep('<',des[rline:(sline-1)])) > 0
   statusLineNum <- slines[1]
   status <- des[length(des)]
   nChars <- min(25,nchar(status))
   status <- substr(status,1,nChars)
   c(HasReviewer,status)
}

######################  autAddr  ################################

# inputs author's e-mail address (can be partial), outputs submission
# number, 20XX-Y; assumes 'subs', output of getAll(), is global; numOnly
# means return the submission number only, else the entire line in subs

autAddr <- function(aut,numOnly=TRUE) {
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

# the 'fileList' argument will be tacked on to 'git add' (or 'git mv' if
# op is 'mv'), with the file names relative to current directory, and
# the git op will be performed 

# example

#   pushToGitHub('xy z','"new src files"')

# will push xy and z in current directory, with the commit done with the
# message "new src files"

pushToGitHub <- function(fileList,commitComment,op='add',mvdest) {
   if (!(op %in% c('add','mv'))) stop('bad op')
   partcmd <- paste('git',op)
   if (op == 'add') cmd <- makeSysCmd(paste(partcmd,fileList))
   else cmd <- makeSysCmd(paste(partcmd,fileList,mvdest))
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

editPush <- function(fname,commitComment) {
   print('make sure commitComment has double quotes within single')
   readline('hit Enter when ready')
   textEditor <- Sys.getenv('EDITOR')
   cmd <- makeSysCmd(textEditor,fname)
   cmd()
   pushToGitHub(fname,commitComment)
}

###########################  sendLetter  ##################################

# send letter to manuscript author

# arguments:
# 
#    msNum: manuscript number
#    template: .R file name, given relative to RJNM_DIR
#    attaches: R vector of file names to be attached, given relative to
#        current working directory

# notes on the template: 

#   This assigns to a global variable 'formletter'  an R character
#   vector, one element per line of the letter. 

#   It is assumed that the recipient's surname, the editor's name, 
#   and title of the paper all appear in line 1 of the template, as
#   fields GREET, EDITOR and TITLE to be substituted.

#   Must have both single and double-quoted subject.

sendLetter <- function(msNum,surname,addr,singdoubsubject,template,attaches) {
   if (!exists('subs')) stop('run getAll() first')
   editorName <- Sys.getenv('RJ_NAME')
   if (nchar(editorName) == 0)
      stop('please set your RJ_NAME environment variable')
   rjnmDir <- Sys.getenv('RJNM_DIR')
   if (nchar(rjnmDir) == 0)
      stop('please set your RJNM_DIR environment variable')
   template <- paste0(rjnmDir,'/',template)
   source(template)  # sets global var 'formletter'
   des <- desFiles[[msNum]]
   formletter[1] <- sub('GREET',surname,formletter[1])
   formletter[1] <- sub('EDITOR',editorName,formletter[1])
   title <- des[1]
   title <- substr(title,8,nchar(title))
   formletter[1] <- sub('TITLE',title,formletter[1])
   formletter <- c(formletter,'\n')
   # check it
   cat(formletter)
   edit <- readline('edit, say for personalizing? ')
   if (substr(edit,1,1) == 'y') {
      formletter <- edit(formletter)
      cat(formletter,file='formletterfile',sep='\n\n')
      system('cat formletterfile')
   }
   # send
   mailIt(addr=addr,singdoubsubject,formletter,attaches=attaches)
}

# mail the message 'ltr' (a character vector, one element per
# line) that we've composed, sending to address 'addr', with subject
# title 'subject'; here 'attaches' is a vector of names of files to be
# attached to the message; note:  'ltr' is written to the file 'tmpltr';
# the subject must be quoted if it contains spaces

mailIt <- function(addr,subject,attaches,ltr,mailer='muttMailer') 
{
   if (mailer != 'muttMailer') stop('only configured to mutt for now')
   mailCmd <- paste('mutt',addr,'-s',subject)
   # for (att in attaches) {
   #    mailCmd <- paste0(mailCmd,' -a "',att,'" ')
   # }
   if (!is.null((attaches)))
      mailCmd <- paste(mailCmd,'-a', attaches)
   unlink('tmpltr')
   writeLines(ltr,con='tmpltr')
   mailCmd <- paste0(mailCmd,' < tmpltr')
   print(mailCmd)
   ans <- readline('OK to send? ')
   if (substr(ans,1,1) != 'y') stop('exiting')
   cmd <- makeSysCmd(mailCmd)
   cmd()
}

# return the last part of the path, e.g. z in /x/y/z
getLocalDirName <- function() {
   wd <- getwd()
   wdparts <- strsplit(wd,split='/')[[1]]  # works even in MSWin
   wdparts[length(wdparts)]
}

