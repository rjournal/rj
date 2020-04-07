
# Many authors include LaTeX packages, macros and so on.  The code here
# extracts these from each RJwrapper.tex, then concatenates them for
# insertion into RJ-issueid.tex (done by hand).  Run from Proofs/issueid.

getOnePreamble <- function(oneArticle) {
   txt <- readLines(paste0(oneArticle, '/RJwrapper.tex'))
   bt <- grep('booktabs', txt)
   if (length(bt) != 1)  {
      warning(paste('booktabs problem', oneArticle))
      return('')
   }
   bd <- grep('document', txt)
   if (length(bd) == 0)  {
      warning(paste('document problem', oneArticle))
      return('')
   }
   bd <- bd[2]
    
   txtOut <- paste('%%', oneArticle)
   txtOut <- c(txtOut, '')
   txtOut <- c(txtOut, txt[(bt + 1):(bd - 1)])
   txtOut <- c(txtOut, '')
 
   txtOut
}

getAllPreambles <- function() {
   dirs <- list.dirs(recursive = FALSE, full.names = FALSE)
   dirs <- dirs[grep('^2', dirs)]
   tmp <- Reduce(c,Map(getOnePreamble, dirs))
   Filter(function(elt) elt[1] != '', tmp)
}

