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

api_get_article <- function(id, auth=NULL) {
  tryCatch(
     list(success=TRUE, article=api_article_info(as.article(id))),
     error=function(e) list(success=FALSE, message=paste0("Error while getting information for article ",id,": ",as.character(e))))
}