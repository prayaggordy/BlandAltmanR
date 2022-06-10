test_that("null.omit() works on vectors", {
  expect_equal(null.omit(c(1, 2, 3, NULL, 4, 5)), c(1, 2, 3, 4, 5))
})

test_that("null.omit() works on flat, unnamed lists", {
	expect_equal(null.omit(list(1, 2, 3, NULL, 4, 5)), list(1, 2, 3, 4, 5))
})

test_that("null.omit() works on flat, named lists", {
	expect_equal(null.omit(list(a = 1, b = 2, c = 3, n = NULL, d = 4, e = 5)),
							 list(a = 1, b = 2, c = 3, d = 4, e = 5))
})
