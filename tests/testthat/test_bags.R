context("Bags")

test_that("We can construct linked lists bag correctly", {
  bag <- bag_cons(1, bag_cons(2, bag_cons(3, empty_list_bag())))
  expect_equal(as.vector(bag, "integer"), 1:3)
})

test_that("We can concatenate two list bags", {
  bag1 <- bag_cons(1, bag_cons(2, bag_cons(3, empty_list_bag())))
  bag2 <- bag_cons(4, bag_cons(5, bag_cons(6, empty_list_bag())))
  bag <- merge(bag1, bag2)
  expect_equal(as.vector(bag, "integer"), 1:6)
  bag <- insert(bag, 7)
  expect_equal(as.vector(bag, "integer"), c(7, 1:6))
})

test_that("We can construct tree bag correctly", {
  bag <- empty_tree_bag()
  for (x in 1:3) bag <- insert(bag, x)
  expect_equal(as.vector(bag, "integer"), 1:3)
})

test_that("We can concatenate two list bags", {
  bag1 <- empty_tree_bag()
  for (x in 1:3) bag1 <- insert(bag1, x)
  bag2 <- empty_tree_bag()
  for (x in 4:6) bag2 <- insert(bag2, x)
  bag <- merge(bag2, bag1) # notice order...
  expect_equal(as.vector(bag, "integer"), 1:6)
})
