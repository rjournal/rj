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
