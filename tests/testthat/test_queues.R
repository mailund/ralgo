context("Queues")

test_queue <- function(empty) {
  q <- empty
  for (x in 1:3)
    q <- enqueue(q, x)

  v <- vector("integer", 3)
  for (i in 1:3) {
    v[i] <- front(q)
    q <- dequeue(q)
  }
  expect_equal(v, 1:3)
  expect_true(is_empty(q))

  for (i in 1:3) {
    q <- enqueue(q, i)
    v[i] <- front(q)
    q <- dequeue(q)
  }
  expect_equal(v, 1:3)
  expect_true(is_empty(q))

  for (x in 1:3)
    q <- enqueue(q, x)
  for (x in 1:3)
    q <- dequeue(q)
  expect_true(is_empty(q))
}

test_that("We can construct and empty an environment-based queue", {
  test_queue(empty_env_queue())
})

test_that("We can construct and empty a closure queue", {
  test_queue(empty_closure_queue())
})

test_that("We can construct and empty an extended queue", {
  test_queue(empty_extended_queue())
})

test_that("We can construct and empty a lazy queue", {
  test_queue(empty_lazy_queue())
})

test_that("We can construct and empty a rebuild queue", {
  test_queue(empty_rebuild_queue())
})
