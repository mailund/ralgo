context("Stacks")

test_that("We can construct a stack correctly", {
  stack <- empty_stack()
  stack <- push(stack, 2)
  stack <- push(stack, 1)
  expect_equal(as.vector(stack, "integer"), 1:2)
})

test_that("We can pop from a stack correctly", {
  stack <- empty_stack()
  stack <- push(stack, 2)
  stack <- push(stack, 1)
  expect_equal(top(stack), 1)
  stack <- pop(stack)
  expect_equal(top(stack), 2)
  stack <- pop(stack)
  expect_true(is_empty(stack))
})
