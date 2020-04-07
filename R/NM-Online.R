
# for EiC to put an accepted paper in 'online' status, meaning it will
# show up on the landing page under "Accepted articles"; note:  does NOT
# copy to SVN, needed for external viewing 

# run from Accepted/

require(gitR)  # github/matloff/gitR

putOnlineNM <- function(msnum) 
{
   options(warn = 1)  # e.g. if bib problem, want to know now

   cat('\n\nprocessing msnum ',msnum,'\n')
   accdir <- getwd()  # Accepted/
   on.exit(setwd(accdir))
   setwd(msnum)

   # if already has slug, skip this one
   slug <- checkForSlugNM()
   if (nchar(slug) > 0) {
      cat(msnum, ' already has slug: ', slug, '\n',sep='')
      return()
   }

   # create supplement if nonexistent and there is an .R file
   des <- checkForSuppsNM()

   print('check .bib files:')
   print(dir(pattern=glob2rx('*.bib')))
   print('should be only 1, plus 1 saved original')
   readline('hit ctrl-C if this is not the case, else Enter to continue ')
   # each run of rj:::find_update_bib() will copy x.bib to orig_x.bib,
   # overwriting the last, thus not really "original"; may be useful to
   # save the real original
   if (length(dir(pattern='orig_')) == 0) {
      bibfile <- dir(pattern='.bib')
      file.copy(bibfile,'REALorigBIB')
   }

   setwd('../..')  # articles/
   print('checking bib')
   readline('Hit Enter when ready ')
   find_update_bib(msnum)

   print('adding to _config.yml')
   readline('Hit Enter when ready ')
   rj::publish(msnum)

   # push DESCRIPTION, *.bib, RJwrapper.pdf, supplementaries.zip,
   # _config.yml, archive/thisyear  to GitHub
   setwd(paste0(accdir,'/',msnum))
   # get name of .bib
   origbib <- dir(pattern=glob2rx('orig_*.bib'))
   bib <- substr(origbib,6,nchar(origbib))
   rjpdf <- 'RJwrapper.pdf'
   suppszip <- 'supplementaries.zip'
   toPush <- paste('DESCRIPTION',origbib,bib,rjpdf,suppszip)
   gitOpPush(toPush,'"for putting online"',quiet=TRUE,acceptEnter=TRUE)
   setwd('../../../rjournal.github.io/')  # different repo, for .yml etc.
   yrID <- format(Sys.Date(), "%Y")
   archiveDir <- paste0('archive/',yrID)
   toPush <- paste('_config.yml',archiveDir)
   gitOpPush(toPush,'"rjournal.github.io stuff"',quiet=TRUE,acceptEnter=TRUE)
   setwd(accdir)
}

# assumes already in ms directory, e.g. 2016-52/
checkForSlugNM <- function() {
   rl <- readLines('DESCRIPTION')
   slug <- rl[grep('Slug',rl)]
   if (length(slug) == 0) return('')
   substr(slug,7,18)
}

# need lo add a Suppl: line for supplementary material?

checkForSuppsNM <- function(msnum) 
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
      gitOpPush('DESCRIPTION','"added Suppl: line"',quiet=TRUE,acceptEnter=TRUE)
      des <- readLines('DESCRIPTION')  # re-read, just to make sure
      print('new des:')
      cat(des,sep='\n')
      ans <- readline('\n\nfurther edit? ')
      if (substr(ans,1,1) == 'y') {
         editPushNM('DESCRIPTION','"further edit"')
         des <- readLines('DESCRIPTION')  # re-read, just in case
      } 
   }
   des
}

