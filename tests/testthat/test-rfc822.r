context("rfc822")

test_that("Addresses parsed into name and email", {
  expect_equal(parse_address("b <a> [Blah]"), address("a", "b", "Blah"))
  expect_equal(parse_address("<a>"), address("a"))
  expect_equal(parse_address("b"), address(NULL, "b"))

  expect_equal(parse_address(" b <a>"), address("a", "b"))
  expect_equal(parse_address("\nb <a>"), address("a", "b"))
  expect_equal(parse_address("b\n<a>"), address("a", "b"))

})
