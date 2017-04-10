context("basics")

test_that("We can construct linked lists correctly", {
  lst <- list_cons(1, list_cons(2, list_cons(3, empty_list())))
  expect_equal(as.vector(lst, "integer"), 1:3)
})


test_that("We can concatenate two lists", {
  l1 <- list_cons(1, list_cons(2, list_cons(3, empty_list())))
  l2 <- list_cons(4, list_cons(5, list_cons(6, empty_list())))
  lst <- list_concatenate(l1, l2)
  expect_equal(as.vector(lst, "integer"), 1:6)
})
