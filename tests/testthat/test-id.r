context("ID")

test_that("Must have two components", {
  expect_error(parse_id("2005"), "must have form")
  expect_error(parse_id("2005-01-01"), "must have form")
})

test_that("Components must be numeric", {
  expect_error(parse_id("2010-x"), "must have form")
  expect_error(parse_id("x-01"), "must have form")
  expect_error(parse_id("x-y"), "must have form")
})

test_that("Year must be plausible", {
  expect_error(parse_id("2000-01"), "must be")
  expect_error(parse_id("3000-01"), "must be")
})

test_that("No partial matches", {
  expect_error(parse_id("a2005-01"), "must have form")
  expect_error(parse_id("a2005-01a"), "must have form")
})

test_that("Whitespace ignored", {
  expect_equal(parse_id("2002-01"), id(2002, 1))
  expect_equal(parse_id("2002-01   "), id(2002, 1))
  expect_equal(parse_id("\t2002-01"), id(2002, 1))
  expect_equal(parse_id("\n2002-01"), id(2002, 1))
})
