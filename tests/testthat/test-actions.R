context("Actions")

test_that("Can't create invalid status", {
  valid <- load_article("valid-article.dcf")

  expect_error(update_status(valid, "blah"), "not a known status")
  expect_error(
    update_status(valid, "rejected", date = "2000-01-01"),
    "before the R journal"
  )
})
