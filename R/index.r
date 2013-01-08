read.index <- function(name = "index.dcf") {
  ix <- as.data.frame(read.dcf(name), stringsAsFactors=FALSE)
  ix$Status <- tolower(ix$Status)

  ix
  # class(ix) <- c("RJindex", class(ix))
  # subset(
  #        within(ix, {
  #          Status <- tolower(Status)
  #          Status[is.na(Status)] <- "need editor"
  #          Editor[is.na(Editor)] <- ""
  #          Stage <- Stage(Status)
  #        }),
  #        Refno != ""
  #        )
}
