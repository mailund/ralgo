
RED <- 1
BLACK <- 2
DOUBLE_BLACK <- 3

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

check_invariants <- function(tree) {
  if (!rbt_invariant_red(tree))
    cat("Red invariant is violated!")
  if (!rbt_invariant_black(tree))
    cat("Black invariant is violated!")
  if (!rbt_invariant_balanced(tree))
    cat("Balance invariant is violated!")
}

tree <- empty_red_black_tree()
for (i in 1:10)
  tree <- insert(tree, i)
plot(tree)

plot(x <- remove(tree, 0)) ; check_invariants(x)
plot(x <- remove(tree, 1)) ; check_invariants(x)
plot(x <- remove(tree, 2)) ; check_invariants(x)
plot(x <- remove(tree, 3)) ; check_invariants(x)
plot(x <- remove(tree, 4)) ; check_invariants(x)
plot(x <- remove(tree, 5)) ; check_invariants(x)
plot(x <- remove(tree, 6)) ; check_invariants(x)
plot(x <- remove(tree, 7)) ; check_invariants(x)
plot(x <- remove(tree, 8)) ; check_invariants(x)
plot(x <- remove(tree, 9)) ; check_invariants(x)
plot(x <- remove(tree, 10)) ; check_invariants(x)
plot(x <- remove(tree, 11)) ; check_invariants(x)
