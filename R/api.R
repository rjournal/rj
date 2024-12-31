## calls a function with arguments parsed from JSON input
api_call <- function(api.fun, fn=NULL) {
  options(cli.record="none") ## disable cli stupidity
  cat(jsonlite::toJSON(
    do.call(api.fun,
      jsonlite::fromJSON(readLines(
          if (is.null(fn)) file('stdin') else fn
        ), simplifyDataFrame=FALSE)
    ), auto_unbox=TRUE, null="null"))
}

api_version <- function(nonce=NULL) {
  list(success=TRUE, api=1L, rj=packageDescription("rj",fields="Version"), nonce=nonce)
}

api_get_article <- function(id, public=TRUE) {
  tryCatch(
     list(success=TRUE, article=api_article_info(as.article(id), public=public)),
     error=function(e) list(success=FALSE, message=paste0("Error while getting information for article ",id,": ",as.character(e))))
}

.update_api_files <- function() {
   ## the CSV files are a mess - we try to make them into a more consistent structure
   ed <- read.csv(system.file("editors.csv", package="rj")) # name, email, github, real
   ae <- read.csv(system.file("associate-editors.csv", package="rj")) # name, initials, github_handle, email, start_year, end_year, keywords
   y <- as.integer(substr(Sys.Date(),1,4))
   ae$active <- ae$start_year >= y & ae$end_year <= y
   ed$active <- seq.int(nrow(ed)) > nrow(ed) - 3
   ed$role <- 'editor'
   ae$role <- 'ae'
   u = rbind(ed, data.frame(name=ae$initials, email=ae$email, github=ae$github_handle, real=ae$name, active=ae$active, role=ae$role))
   ## remove anyone without GH handle (can't authenticate)
   u <- u[!is.na(u$github) & nzchar(u$github),]
   writeLines(jsonlite::toJSON(u), "rjusers.json")
}
