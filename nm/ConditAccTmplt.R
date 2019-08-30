
# most will be Dr. Someone, and the rest won't mind being called Dr. :-)

formletter <<- 
c(
'\n\nHello, Dr. GREET.  EDITOR of the R Journal editorial board here.
I am happy to inform you that I am accepting your paper, TITLE, for publication, subject to your addressing the reviewer comments, attached.', 
"Please send me your revised files -- ONLY the ones that you change, please -- as well as an itemized document -- plain text or PDF, please -- explaining the changes you made, and the reasons for declining to make some of the suggested changes.",
"Please do NOT highlight your changes.",
"In your revised files, please make sure to continue to comply with R Journals formatting rules etc.",
"In particular: Only 1 .bib file; only 1 .tex file other than RJwrapper.tex; no \cite's of the R language, and no such listing in .bib.",
paste0('\n',Sys.getenv('RJ_NAME'))
)

