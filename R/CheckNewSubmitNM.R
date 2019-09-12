
# various checks for readiness to review

# run from articles/

checkNewSubmit <- function(msnum) {
   setwd(paste0('Submissions/',msnum))
   # finicky rj software can only allow for 2 .tex and 1 .bib
   if (length(dir(pattern=glob2rx('*.tex*'))) > 2)
      stop('only 2 *.tex* allowed')
   if (length(dir(pattern=glob2rx('*.bib*'))) > 1)
      stop('only 1 *.bib* allowed')
   # and only 1 ref to .tex in RJwrapper.tex, even in comments
   texs <- grep('.tex',readLines('RJwrapper.tex'))
   if (length(texs) > 1)
      stop('only 1 mention allowed in RJwrapper.tex of ".tex"')
   # run LaTeX checks
   rj::build_latex('RJwrapper.tex')
}

