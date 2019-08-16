
# for EiC to put an accepted paper in 'online' status, meaning it will
# show up on the landing page under "Accepted articles"; note:  does NOT
# copy to SVN, needed for external viewing

# run from Accepted/

require(gitR)  # github/matloff/gitR

putOnline <- function(msnum) 
{

   options(warn = 1)  # e.g. if bib problem, want to know now

   cat('\n\nprocessing msnum ',msnum,'\n')
   accdir <- getwd()  # Accepted/
   on.exit(setwd(accdir))

   setwd(msnum)
   des <- checkForSupps()

   setwd('../..')  # articles/
   print('checking bib')
   readline('Hit Enter when ready ')
   rj:::find_update_bib(msnum)

   print('adding to _config.yml')
   readline('Hit Enter when ready ')
   rj::publish(msnum)

   # push DESCRIPTION, .bib, _config.yml to GitHub
   setwd(paste0(accdir,'/',msnum))
   # get name of .bib
   bib <- dir(pattern=glob2rx('orig_*.bib'))
   bib <- substr(bib,6,nchar(bib))
   gitOpPush(paste('DESCRIPTION',bib),'"for putting online"')
   setwd('../../../rjournal.github.io/')  # different repo
   gitOpPush('_config.yml','"for putting online"')
   setwd(accdir)
}

# need to add a Suppl: line for supplementary material?

checkForSupps <- function(msnum) 
{
   des <- readLines('DESCRIPTION')
   # for now, treat only .R as supp, and assume just 1 such file
   if (length(grep('Suppl:',des)) == 0) {
      # for now, treat only .R as supp, and assume just 1 such file
      stLineNum <- grep('Status:',des)
      suLine <- paste('Suppl:',dir(pattern=glob2rx('*.R')))
      desNew <- c(des[1:(stLineNum-1)],
                  suLine,
                  des[(stLineNum):length(des)])
      writeLines(desNew,con='DESCRIPTION')
      gitOpPush('DESCRIPTION','"added Suppl: line"')
      des <- readLines('DESCRIPTION')  # re-read, just to make sure
      print('new des:')
      cat(des,sep='\n')
      ans <- readline('\n\nfurther edit? ')
      if (substr(ans,1,1) == 'y') {
         editPush('DESCRIPTION','"further edit"')
         des <- readLines('DESCRIPTION')  # re-read, just in case
      } 
   }
   des
}

