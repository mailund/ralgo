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


test_that("We can construct and access a splay tree", {
  tree <- empty_splay_tree()
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


rbt_invariant_red <- function(tree) {
  if (is_empty(tree))
    return(TRUE)
  if (tree$colour == RED && (tree$left$colour == RED || tree$right$colour == RED))
    return(FALSE)
  return(rbt_invariant_red(tree$left) && rbt_invariant_red(tree$right))
}

min_max_black_depth <- function(tree) {
  if (is_empty(tree)) {
    # empty trees are black
    stopifnot(tree$colour == BLACK)
    return(c(1, 1))
  }

  left_min_max <- min_max_black_depth(tree$left)
  right_min_max <- min_max_black_depth(tree$right)
  this_depth <- ifelse(tree$colour == BLACK, 1, 0)
  min_depth <- min(left_min_max[1], right_min_max[1]) + this_depth
  max_depth <- max(left_min_max[2], right_min_max[2]) + this_depth
  c(min_depth, max_depth)
}

rbt_invariant_black <- function(tree) {
  min_max <- min_max_black_depth(tree)
  min_max[1] == min_max[2]
}

min_max_depth <- function(tree) {
  if (is_empty(tree))
    return(c(0, 0))

  left_min_max <- min_max_depth(tree$left)
  right_min_max <- min_max_depth(tree$right)
  min_depth <- min(left_min_max[1], right_min_max[1]) + 1
  max_depth <- max(left_min_max[2], right_min_max[2]) + 1
  c(min_depth, max_depth)
}

rbt_invariant_balanced <- function(tree) {
  depth_range <- min_max_depth(tree)
  2 * depth_range[1] >= depth_range[2]
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

  # check invariants ---
  # ordered...
  tree <- empty_red_black_tree()
  for (elm in 1:10)
    tree <- insert(tree, elm)
  expect_true(rbt_invariant_red(tree))
  expect_true(rbt_invariant_black(tree))
  expect_true(rbt_invariant_balanced(tree))

  # reverse ordered
  tree <- empty_red_black_tree()
  for (elm in rev(1:1000))
    tree <- insert(tree, elm)
  expect_true(rbt_invariant_red(tree))
  expect_true(rbt_invariant_black(tree))
  expect_true(rbt_invariant_balanced(tree))

  # random order
  for (iteration in 1:5) {
    tree <- empty_red_black_tree()
    for (elm in sample(1:200))
      tree <- insert(tree, elm)
    expect_true(rbt_invariant_red(tree))
    expect_true(rbt_invariant_black(tree))
    expect_true(rbt_invariant_balanced(tree))
  }

  # with deletion...
  make_increasing <- function() {
    tree <- empty_red_black_tree()
    for (i in 1:100)
      tree <- insert(tree, i)
    tree
  }
  make_random <- function() {
    tree <- empty_red_black_tree()
    for (i in sample(1:100))
      tree <- insert(tree, i)
    tree
  }
  for (iteration in 1:5) {
    tree <- make_increasing()
    for (elm in sample(1:100, size = 50)) # remove half the elements
      tree <- remove(tree, elm)
    # remove some non-existing elements as well...
    for (elm in sample(200:300, size = 5)) # remove half the elements
      tree <- remove(tree, elm)

    expect_true(rbt_invariant_red(tree))
    expect_true(rbt_invariant_black(tree))
    expect_true(rbt_invariant_balanced(tree))

    tree <- make_random()
    for (elm in sample(1:100, size = 50)) # remove half the elements
      tree <- remove(tree, elm)
    expect_true(rbt_invariant_red(tree))
    expect_true(rbt_invariant_black(tree))
    expect_true(rbt_invariant_balanced(tree))
  }

})
