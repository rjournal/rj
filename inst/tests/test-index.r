context("Index")

test_that("Unparseable blocks written out as is", {
  ind <- load_index("index-parse-fail.dcf", quiet = TRUE)
  raw <- paste(readLines("index-parse-fail.dcf"), collapse = "\n")

  expect_equal(raw, format(ind))
})
