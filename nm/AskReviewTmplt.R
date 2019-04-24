
formletter <<- c(
   "\n\nHello, GREET.  EDITOR of the R Journal editorial board here.  I am writing to you in my capacity as an editor on the R Journal, and I have a paper that I hope you might have time to review, TITLE.", 
   "The R Journal is not mathematical. The major points a referee should address are questions such as: Does the software look useful to a reasonably broad audience, and have the authors made a good case for this?  Are there enhancements or changes that the authors should consider?  Is the presentation clear, and (this is very important) have the authors sufficiently explained the methodology used?  Are the references complete? Etc.  The referee of course need not verify the correctness of the code.",
   "The package is on CRAN.",
   "The paper and any supplementary code are attached.",
   "Please let me know soon whether you will be able to take on this assignment.  I would like have your review by DUEDATE, though of course a somewhat later submission would be OK. Please submit in either plain text or PDF, not Word. Thanks in advance for your consideration.",
paste0('\n',Sys.getenv('RJ_NAME'))
)

