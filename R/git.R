## calls git with the given arguments. No path expansion is done,
## but all arguments are quoted. All arguments are unfolded, i.e.,
## they don't need to be scalar but the result will be single
## character vector sequence.
## If GIT is not set it defaults to "git"
git <- function(..., git=Sys.getenv("GIT")) {
  if (!nzchar(git)) git <- "git"
  args <- sapply(as.character(unlist(list(...))), shQuote)
  system2(git, args)
}
