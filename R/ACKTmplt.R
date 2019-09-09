
# most will be Dr. Someone, and the rest won't mind being called Dr. :-)

edtr <- Sys.getenv('RJ_NAME')

tmp <- paste0('\n\nHello, Dr. GREET.  ',edtr,
   ' of the R Journal editorial board here.  ',
   'Thanks for your submission, TITLE.')    

formletter <<- 
c(tmp,
'Please note that we now require that authors provide all real datasets used in their examples.",
'The review process can range from weeks to months, depending on the ease of securing good reviewers.  Please do not hesitate to contact me during that process if you have any concerns.',
paste0('\n',edtr)
)

