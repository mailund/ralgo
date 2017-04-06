context("Search trees")

test_that("We can construct and access a red-black search tree", {
  tree <- empty_red_black_tree()
  expect_true(is_empty(tree))
  expect_false(member(tree, 1))

  tree <- insert(tree, 1)
  expect_false(is_empty(tree))
  expect_true(member(tree, 1))
  expect_false(member(tree, 5))

  tree <- insert(tree, 5)
  expect_false(is_empty(tree))
  expect_true(member(tree, 1))
  expect_true(member(tree, 5))

  for (elm in 1:10) {
    tree <- insert(tree, elm)
  }

  expect_false(member(tree, 0))
  for (elm in 1:10) expect_true(member(tree, elm))
  expect_false(member(tree, 11))

  for (elm in 1:10) {
    tree <- remove(tree, elm)
  }
  for (elm in 1:10) expect_false(member(tree, elm))

})
