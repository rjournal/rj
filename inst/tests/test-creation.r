context("Creation")

ind <- index(list(make_article(id(year(), 1))))

test_that("First ID is 1", {
  empty <- index(list())
  expect_equal(new_id(empty)$seq, 1)

  last_year <- index(list(make_article(id(2000, 1))))
  expect_equal(new_id(empty)$seq, 1)
})

test_that("Next ID increments max by 1", {
  ten <- index(list(make_article(id(year(), 10))))
  expect_equal(new_id(ten)$seq, 11)
})
