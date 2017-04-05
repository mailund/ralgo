context("basics")

test_that("We can construct linked lists correctly", {
  lst <- list_cons(1, list_cons(2, list_cons(3, empty_list())))
  expect_equal(as.vector(lst, "integer"), 1:3)
})
