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

test_that("We can construct and access a leftist heap", {
  test_heap(empty_leftist_heap())
})
