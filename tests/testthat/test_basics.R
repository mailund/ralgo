context("Basics")

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

test_that("We can remove elements from lists", {
  lst <- list_cons(1, list_cons(2, list_cons(3, empty_list())))
  lst <- remove(lst, 2)
  expect_equal(as.vector(lst, "integer"), c(1,3))
})

test_that("We can get and remove n elements", {
  lst <- list_cons(1, list_cons(2, list_cons(3, list_cons(4, empty_list()))))
  l1 <- list_get_n(lst, 2)
  l2 <- list_drop_n(lst, 2)
  expect_equal(as.vector(l1, "integer"), 1:2)
  expect_equal(as.vector(l2, "integer"), 3:4)

  l1 <- list_get_n(lst, 1)
  l2 <- list_drop_n(lst, 1)
  expect_equal(as.vector(l1, "integer"), 1)
  expect_equal(as.vector(l2, "integer"), 2:4)

  l1 <- list_get_n(lst, 3)
  l2 <- list_drop_n(lst, 3)
  expect_equal(as.vector(l1, "integer"), 1:3)
  expect_equal(as.vector(l2, "integer"), 4)

})