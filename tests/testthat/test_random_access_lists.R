context("Random access lists")

ral_size <- function(ral) {
  size <- 0
  while (!is.null(ral)) {
    size <- size + ral$tree_size
    ral <- ral$siblings
  }
  size
}

ral_to_vector <- function(ral) {
  v <- vector(typeof(ral$tree$value), ral_size(ral))

  dfs <- function(tree, tree_size, idx) {
    if (!is.null(tree)) {
      v[idx] <<- tree$value
      child_size <- (tree_size - 1) / 2
      dfs(tree$left, child_size, idx + 1)
      dfs(tree$right, child_size, idx + 1 + child_size)
    }
  }

  idx <- 1
  while (!is.null(ral)) {
    dfs(ral$tree, ral$tree_size, idx)
    idx <- idx + ral$tree_size
    ral <- ral$siblings
  }
  v
}

test_that("We can construct lists correctly", {
  lst <- ral_cons(1, NULL)
  expect_equal(ral_to_vector(lst), 1)

  lst <- ral_cons(1, ral_cons(2, NULL))
  expect_equal(ral_to_vector(lst), 1:2)

  lst <- ral_cons(1, ral_cons(2, ral_cons(3, NULL)))
  expect_equal(ral_to_vector(lst), 1:3)

  lst <- ral_cons(1, ral_cons(2, ral_cons(3, ral_cons(4, NULL))))
  expect_equal(ral_to_vector(lst), 1:4)

  lst <- ral_cons(1, ral_cons(2, ral_cons(3, ral_cons(4, ral_cons(5, NULL)))))
  expect_equal(ral_to_vector(lst), 1:5)

  lst <- ral_cons(1, ral_cons(2, ral_cons(3, ral_cons(4, ral_cons(5, ral_cons(6, NULL))))))
  expect_equal(ral_to_vector(lst), 1:6)
})


test_that("We can access heads and tails", {
  lst <- ral_cons(1, ral_cons(2, ral_cons(3, ral_cons(4, ral_cons(5, ral_cons(6, NULL))))))

  expect_equal(ral_to_vector(lst), 1:6)
  expect_equal(ral_head(lst), 1)

  lst <- ral_tail(lst)
  expect_equal(ral_to_vector(lst), 2:6)
  expect_equal(ral_head(lst), 2)

  lst <- ral_tail(lst)
  expect_equal(ral_to_vector(lst), 3:6)
  expect_equal(ral_head(lst), 3)

  lst <- ral_tail(lst)
  expect_equal(ral_to_vector(lst), 4:6)
  expect_equal(ral_head(lst), 4)

  lst <- ral_tail(lst)
  expect_equal(ral_to_vector(lst), 5:6)
  expect_equal(ral_head(lst), 5)

  lst <- ral_tail(lst)
  expect_equal(ral_to_vector(lst), 6)
  expect_equal(ral_head(lst), 6)

  lst <- ral_tail(lst)
  expect_true(is.null(lst))
})


test_that("We can access random indices", {
  for (n in 1:10) {
    lst <- NULL
    for (i in 1:n)
      lst <- ral_cons(n - i + 1, lst)
    expect_equal(ral_to_vector(lst), 1:n)
    for (i in 1:n)
      expect_equal(i, ral_lookup(lst, i))
  }

  expect_error(ral_lookup(lst, 11), "Index out of bounds")
})

test_that("We can update at random indices", {
  for (n in 1:6) {
    lst <- NULL
    for (i in 1:n)
      lst <- ral_cons(n - i + 1, lst)
    expect_equal(ral_to_vector(lst), 1:n)

    for (i in 1:n) {
      expected_output <- 1:n
      expected_output[i] <- -1
      expect_equal(ral_to_vector(ral_update(lst, i, -1)), expected_output)
    }

  }
  expect_error(ral_lookup(lst, 7), "Index out of bounds")
  expect_error(ral_update(lst, 0, NA), "Index out of bounds")
  expect_error(ral_update(lst, 7, NA), "Index out of bounds")
})
