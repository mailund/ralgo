context("Search trees")

test_that("We can construct and access a (unbalanced) search tree", {
  tree <- empty_search_tree()
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
    expect_true(member(tree, elm))
    tree <- remove(tree, elm)
    expect_false(member(tree, elm))
  }
  for (elm in 1:10)
    expect_false(member(tree, elm))
  expect_true(is_empty(tree))

  for (elm in sample(1:10)) {
    tree <- insert(tree, elm)
  }
  for (elm in sample(1:10)) {
    expect_true(member(tree, elm))
    tree <- remove(tree, elm)
    expect_false(member(tree, elm))
  }
})

min_max_depth <- function(tree) {
  if (is_empty(tree))
    return(c(0, 0))

  left_min_max <- min_max_depth(tree$left)
  right_min_max <- min_max_depth(tree$right)
  min_depth <- min(left_min_max[1], right_min_max[1]) + 1
  max_depth <- max(left_min_max[2], right_min_max[2]) + 1
  c(min_depth, max_depth)
}

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

  tree <- empty_red_black_tree()
  for (elm in 1:10) {
    tree <- insert(tree, elm)
  }

  expect_false(member(tree, 0))
  for (elm in 1:10) expect_true(member(tree, elm))
  expect_false(member(tree, 11))

  for (elm in 1:10) {
    expect_true(member(tree, elm))
    tree <- remove(tree, elm)
    expect_false(member(tree, elm))
  }

  for (elm in 1:10)
    expect_false(member(tree, elm))
  expect_true(is_empty(tree))


  for (elm in sample(1:100)) {
    tree <- insert(tree, elm)
  }
  for (elm in sample(1:100)) {
    expect_true(member(tree, elm))
    tree <- remove(tree, elm)
    expect_false(member(tree, elm))
  }


  # check balanced-ness ----

  # ordered...
  tree <- empty_red_black_tree()
  for (elm in 1:1000)
    tree <- insert(tree, elm)
  depth_range <- min_max_depth(tree)
  expect_true(2 * depth_range[1] >= depth_range[2])

  # reverse
  tree <- empty_red_black_tree()
  for (elm in rev(1:1000))
    tree <- insert(tree, elm)
  depth_range <- min_max_depth(tree)
  expect_true(2 * depth_range[1] >= depth_range[2])

  # random order
  for (iteration in 1:10) {
    tree <- empty_red_black_tree()
    for (elm in sample(1:200))
      tree <- insert(tree, elm)
    depth_range <- min_max_depth(tree)
    expect_true(2 * depth_range[1] >= depth_range[2])
  }
})
