context("Queues")

test_that("We can construct and empty an environment-based queue", {
  q <- empty_env_queue()
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
})

test_that("We can construct and empty a closure queue", {
  q <- empty_closure_queue()
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
})

test_that("We can construct and empty an extended queue", {
  q <- empty_extended_queue()
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
})
