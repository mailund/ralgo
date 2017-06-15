
ral_binary_tree <- function(value, left, right) {
  list(value = value, left = left, right = right)
}

ral_node <- function(tree, tree_size, siblings) {
  list(tree = tree, tree_size = tree_size, siblings = siblings)
}

ral_singleton_node <- function(value, siblings = NULL) {
  singleton_tree <- ral_binary_tree(value, NULL, NULL)
  ral_node(singleton_tree, 1, siblings)
}

#' Check if a random access list is empty
#'
#' @param ral Random access list
#' @return whether ral is empty
#' @export
ral_is_empty <- function(ral) is.null(ral)

#' Construct a new random access list by prepending an element.
#'
#' Empty random access lists are represented as NULL, so to create lists
#' start from there.
#'
#' @param elem Element to put at the front of the list
#' @param ral  Random access list
#' @return New random access list with elem at the front
#' @export
ral_cons <- function(elem, ral) {
  if (ral_is_empty(ral) || ral_is_empty(ral$siblings))
      return(ral_singleton_node(elem, ral))

  first <- ral$tree
  first_size <- ral$tree_size
  second <- ral$siblings$tree
  second_size <- ral$siblings$tree_size
  rest <- ral$siblings$siblings

  if (first_size < second_size)
    ral_singleton_node(elem, ral)
  else
    ral_node(ral_binary_tree(elem, first, second), first_size + second_size + 1, rest)
}

#' Get the front element of a random access list
#'
#' @param ral Random access list
#' @return The front element
#' @export
ral_head <- function(ral) {
  ral$tree$value
}

ral_is_singleton <- function(ral) {
  ral$tree_size == 1
}

#' Get the tail of a random access list
#'
#' @param ral Random access list
#' @return The tail of the list
#' @export
ral_tail <- function(ral) {
  if (ral_is_singleton(ral))
    ral$siblings
  else {
    left <- ral$tree$left
    right <- ral$tree$right
    size <- (ral$tree_size - 1) / 2
    ral_node(left, size, ral_node(right, size, ral$siblings))
  }
}

ral_is_leaf <- function(tree)
  !is.null(tree) && is.null(tree$left) && is.null(tree$right)

ral_tree_lookup <- function(tree, tree_size, idx) {
  stopifnot(idx > 0)

  if (idx == 1) return(tree$value)

  if (ral_is_leaf(tree)) # a leaf but idx is not one!
    if (idx > 1) stop("Index error in lookup")

  child_size <- (tree_size - 1) / 2
  if (idx <= child_size + 1)
    ral_tree_lookup(tree$left, child_size, idx - 1)
  else
    ral_tree_lookup(tree$right, child_size, idx - 1 - child_size)
}

#' Get the value at a given index of a random access list.
#'
#' @param ral The random access list
#' @param idx The index to access
#' @return The value at index idx
#' @export
ral_lookup <- function(ral, idx) {
  while (!is.null(ral)) {
    if (idx <= ral$tree_size)
      return(ral_tree_lookup(ral$tree, ral$tree_size, idx))
    idx <- idx - ral$tree_size
    ral <- ral$siblings
  }
  stop("Index out of bounds")
}


ral_tree_update <- function(tree, tree_size, idx, value) {
  if (is.null(tree)) stop("Index error")

  if (ral_is_leaf(tree)) {
    if (idx == 1)
      return(ral_binary_tree(value, NULL, NULL))
    # a leaf but idx is not one!
    stop("Index error")
  }

  if (idx == 1) {
    ral_binary_tree(value, tree$left, tree$right)
  } else {
    child_size <- (tree_size - 1) / 2
    if (idx <= child_size + 1) {
      ral_binary_tree(tree$value,
                      ral_tree_update(tree$left, child_size, idx - 1, value),
                      tree$right)
    } else {
      ral_binary_tree(tree$value,
                      tree$left,
                      ral_tree_update(tree$right, child_size, idx - 1 - child_size, value))
    }
  }
}

#' Update the value at a given index of a random access list.
#'
#' @param ral The random access list
#' @param idx The index to access
#' @param value The new value to put into the list
#' @return The value at index idx
#' @export
ral_update <- function(ral, idx, value) {
  if (idx < 1) stop("Index out of bounds")
  if (idx <= ral$tree_size)
    ral_node(ral_tree_update(ral$tree, ral$tree_size, idx, value),
             ral$tree_size, ral$siblings)
  else
    ral_node(ral$tree, ral$tree_size,
             ral_update(ral$siblings, idx - ral$tree_size, value))

}
