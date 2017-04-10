context("Bags")

test_that("We can construct linked lists bag correctly", {
  bag <- bag_cons(1, bag_cons(2, bag_cons(3, empty_list_bag())))
  expect_equal(as.vector(bag, "integer"), 1:3)
})

test_that("We can concatenate two list bags", {
  bag1 <- bag_cons(1, bag_cons(2, bag_cons(3, empty_list_bag())))
  bag2 <- bag_cons(4, bag_cons(5, bag_cons(6, empty_list_bag())))
  bag <- list_concatenate(bag1, bag2)
  expect_equal(as.vector(bag, "integer"), 1:6)
})
