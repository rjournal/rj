
# various checks for readiness to review

# run from article's directory

checkNewSubmit <- function() 
{
   # rj software can only allow for 2 .tex and 1 .bib
   if (length(dir(pattern=glob2rx('*.tex*'))) > 2)
      warnReArticle('only 2 *.tex* allowed')
   if (length(dir(pattern=glob2rx('*.bib*'))) > 1)
      warnReArticle('only 1 *.bib* allowed')
   # and only 1 ref to .tex in RJwrapper.tex, even in comments
   texs <- grep('.tex',readLines('RJwrapper.tex'))
   if (length(texs) > 1)
      warnReArticle('only 1 mention allowed in RJwrapper.tex of ".tex"')
   # run LaTeX check
   tryCatch(
      tools::texi2pdf("RJwrapper.tex", clean = FALSE, texinput = "..../share")
   )

}

# calls warning() but with the manuscript number

warnReArticle <- function(msg) 
{
   tmp <- getwd()
   tmp <- strsplit(tmp,'/')[[1]]
   msNum <- tmp[length(tmp)]
   warning(paste0(msNum,': ',msg))
}
