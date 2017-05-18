context("Heaps")

test_heap <- function(empty) {
  heap <- empty
  expect_true(is_empty(heap))

  heap <- insert(heap, 3)
  expect_false(is_empty(heap))
  expect_equal(find_minimal(heap), 3)

  heap <- insert(heap, 5)
  expect_equal(find_minimal(heap), 3)

  heap <- insert(heap, 2)
  expect_equal(find_minimal(heap), 2)

  heap <- insert(heap, 1)
  expect_equal(find_minimal(heap), 1)

  heap <- delete_minimal(heap)
  expect_equal(find_minimal(heap), 2)

  heap <- delete_minimal(heap)
  expect_equal(find_minimal(heap), 3)

  heap <- delete_minimal(heap)
  expect_equal(find_minimal(heap), 5)

  heap <- delete_minimal(heap)
  expect_true(is_empty(heap))
}

test_mixing_operations <- function(empty) {
  heap <- empty
  for (i in 1:10) heap <- insert(heap, i)
  for (i in 1:5) heap <- delete_minimal(heap)
  for (i in 11:15) heap <- insert(heap, i)

  v <- vector("numeric", length = 10)
  for (i in 1:10) {
    v[i] <- find_minimal(heap)
    heap <- delete_minimal(heap)
  }

  expect_true(is_empty(heap))
  expect_equal(v, 6:15)
}

test_merging <- function(empty) {
  h1 <- vector_to_heap(1:3, empty)
  h2 <- vector_to_heap(4:6, empty)
  heap <- merge(h1, h2)

  v <- vector("numeric", length = 6)
  for (i in 1:6) {
    v[i] <- find_minimal(heap)
    heap <- delete_minimal(heap)
  }
  expect_true(is_empty(heap))
  expect_equal(v, 1:6)
}


test_that("We can construct and access a leftist heap", {
  test_heap(empty_leftist_heap())
  test_mixing_operations(empty_leftist_heap())
  test_merging(empty_leftist_heap())
})

test_that("We can construct and access a binomial heap", {
  test_heap(empty_binomial_heap())
  test_mixing_operations(empty_binomial_heap())
  test_merging(empty_binomial_heap())
})
