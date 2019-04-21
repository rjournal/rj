

######################  askReview  ############################

# solicit review

# run from Submissions/

# arguments:

#   reviewerAddr:  e-mail address of possible reviewer
#   salut:  salutation, e.g. "Dr. Jones" or "Mike"
#   msNum:  manuscript number
#   cran:  if TRUE, package is on CRAN
#   dueDate:  tentative due date for review
#   attaches:  character string with file numbers, separated by spaces

# starts with form letter, in character vector 'formletter', builds up
# the full letter by substituting from environment variables, and mails
# it

askReview <- function(reviewerAddr,salut,msNum,cran,dueDate,attaches) {

   # get in-memory DESCRIPTION file, check it's the right one
   if (is.null(desFiles))  {
      getAll()
      print('desFiles regenerated')
   }
   des <- desFiles[[msNum]]
   cat('\ncheck correct ms!\n')
   print(des)
   ans <- readline('correct file (yes or no) ')
   if (ans != 'yes') stop('wrong file')

   # construct letter
   # get form version
   rjnmDir <- Sys.getenv('RJNM_DIR')
   source(paste0(rjnmDir,'/askReviewTmplt.R')) 
   # determine article title 
   paperTitle <- getTitle(des)
   # insert title, due date, CRAN status into form letter
   formletter[1] <- sub('TITLE',paperTitle,formletter[1])
   formletter[5] <- sub('DUEDATE',dueDate,formletter[5])
   if (!cran) formletter <- formletter[-3]
   # check it
   print(formletter)
   if (readline('edit, say for personalizing? ') == 'y') {
      formletter <- edit(formletter)
      print(formletter)
   }
   # build mail command
   mailIt(reviewerAddr,'possible refereeing',attaches,formletter,
      mailer='muttMailer')
}

