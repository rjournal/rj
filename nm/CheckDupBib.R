
# 'rj' forms an issue by pooling all the author user files, under a
# single RJwrapper; the .bib files are pooled too, causing a problem if
# there are duplicates across papers; this function finds all such dups

# 'rj' function running at an earlier stage in the process copied the
# author's x.bib to orig_x.bib; the latter must be skipped 

# run from Proofs/year-issueNum

# assumes each article directory contains only one .bib file

# returns a data frame, with columns for the article number, basename of
# the .bib file and duplicated .bib line

checkDupBib <- function() {

   # easier to not use lapply() etc.
   
   # article files
   aFls <- dir()
   tmp <- which(substr(aFls,1,1) == '2')
   aFls <- aFls[tmp]

   outDF <- data.frame()
   for (fl in aFls) {
      bibFl <- paste0(fl,'/*.bib')
      # look for @ lines, but skip the orig_*bib
      bibEntries <- 
         system(paste('grep @',bibFl,'| grep -v "orig_"'),intern=T)
      # each element now has the form 'year-id/basename.bib:bibLine';
      # need to split into three fields
      for (i in 1:length(bibEntries)) {
         be <- bibEntries[i]
         be <- sub('/',' ',be)
         be <- sub(':',' ',be)
         be <- sub('.bib','',be)
         bibEntries[i] <- be
      }

      # form a data frame from this, and tack on to the running data
      # frame
      tmp <- strsplit(bibEntries,' ')
      localDF <- Reduce(rbind,tmp)
      localDF <- as.data.frame(localDF)
      names(localDF) <- c('articleNum','articleBasename','bibLine')
      outDF <- rbind(outDF,localDF)
   }

   # sort data frame and find dups
   idxs <- order(outDF$bibLine)
   outDF <- outDF[idxs,]
   dups <- which(duplicated(outDF$bibLine))
   outDF <- outDF[dups,]

   # finally, want it sorted by file name
   idxs <- order(outDF$articleNum)
   outDF[idxs,]

}

