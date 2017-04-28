context("Deques")

test_that("We can construct and empty a deque", {

  #inserting from the back and getting from the front
  q <- empty_env_deque()
  for (x in 1:3)
    q <- enqueue_back(q, x)

  v <- vector("integer", 3)
  for (i in 1:3) {
    v[i] <- front(q)
    q <- dequeue_front(q)
  }
  expect_equal(v, 1:3)
  expect_true(is_empty(q))

  #inserting from the front and getting from the back
  q <- empty_env_deque()
  for (x in 1:3)
    q <- enqueue_front(q, x)

  v <- vector("integer", 3)
  for (i in 1:3) {
    v[i] <- back(q)
    q <- dequeue_back(q)
  }
  expect_equal(v, 1:3)
  expect_true(is_empty(q))

  #inserting from the front and getting from the front
  q <- empty_env_deque()
  for (x in 1:3)
    q <- enqueue_front(q, x)

  v <- vector("integer", 3)
  for (i in 1:3) {
    v[i] <- front(q)
    q <- dequeue_front(q)
  }
  expect_equal(v, rev(1:3))
  expect_true(is_empty(q))

  #inserting from the back and getting from the back
  q <- empty_env_deque()
  for (x in 1:3)
    q <- enqueue_back(q, x)

  v <- vector("integer", 3)
  for (i in 1:3) {
    v[i] <- back(q)
    q <- dequeue_back(q)
  }
  expect_equal(v, rev(1:3))
  expect_true(is_empty(q))

})
