# read article *.bib, replace by url to https://doi.org/<doi>

update_bib <- function(bibfile, verbose=TRUE) {
  safe <- paste0("orig_", bibfile)
  file.copy(bibfile, safe, overwrite=TRUE)
  biblist <- read.bib(safe)
  fixed_biblist <- fix_bib(biblist)
  for(i in 1:length(fixed_biblist)) {
    if(!is.null(fixed_biblist[i]$doi)) {
      if (!is.null(fixed_biblist[i]$url)) fixed_biblist[i]$url <- NULL
      fixed_biblist[i]$url <- paste0("https://doi.org/", fixed_biblist[i]$doi)
      fixed_biblist[i]$doi <- NULL
    }
  }
  write.bib(fixed_biblist, file=bibfile)
  invisible(fixed_biblist)
}

