
# most will be Dr. Someone, and the rest won't mind being called Dr. :-)

formletter <<- 
c(
"\n\nHello, Dr. GREET.  EDITOR of the R Journal editorial board here.  We are now preparing to publish our next issue.  I am attaching the PDF of your paper, TITLE.  I would like to have your confirmation of the PDF by DEADLINE.",
"The PDF should be the same as the latest one you submitted, save for correction of minor typos.  You may make only small changes now; if in doubt, please check with me.",
paste0('\n',Sys.getenv('RJ_NAME'))
)

