context("Status")

test_that("parsing matches manual creation", {
  expect_equal(parse_status("2005-01-01 Accepted [test]"),
    status("Accepted", as.Date("2005-01-01"), "test"))
  expect_equal(parse_status("2005-01-01 Accepted"),
    status("Accepted", as.Date("2005-01-01")))
})

test_that("status must have at least two components", {
  expect_error(parse_status("2005-01-01"), "must have form")
  expect_error(parse_status("accepted"), "must have form")
})

test_that("first component must be valid date", {
  expect_error(parse_status("2010-01-99 accepted"), "valid date")
  expect_error(parse_status("2010-02-30 accepted"), "valid date")
  expect_error(parse_status("2000-01-01 accepted"), "not before")
  expect_error(parse_status("3000-01-01 accepted"), "in the future")
})

test_that("non-standard statuses give warnings", {
  expect_warning(parse_status("2010-01-01 accept"), "accepted")
  expect_warning(parse_status("2010-01-01 minor"), "minor revision")

})
