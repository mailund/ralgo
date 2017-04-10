context("Queues")

test_that("We can construct and empty a queue", {
  q <- empty_queue()
  for (x in 1:3)
    q <- enqueue(q, x)

  v <- vector("integer", 3)
  for (i in 1:3) {
    v[i] <- front(q)
    q <- dequeue(q)
  }
  expect_equal(v, 1:3)
})
