
# 'rj' forms an issue by pooling all the author user files, under a
# single RJwrapper; the .bib files are pooled too, causing a problem if
# there are duplicates across papers; this function finds all such dups

# 'rj' function running at an earlier stage in the process copied the
# author's x.bib to orig_x.bib; the latter must be skipped 

# run from Proofs/year-issueNum

# assumes each article directory contains only one .bib file

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
      tmp <- strsplit(bibEntries,'@')
      localDF <- Reduce(rbind,tmp)
      outDF <- rbind(outDF,localDF)
   }
   names(outDF) <- c('fname','bibentry')

   # sort data frame
   idxs <- order(outDF$bibentry)
   outDF <- outDF[idxs,]

   dups <- which(duplicated(outDF$bibentry))
   outDF[dups,]
}

