## Generic heap functions ####################

#' Get the minimal value in a heap
#' @param heap The heap
#' @return The minimal value in the heap
#' @export
find_minimal <- function(heap) UseMethod("find_minimal")

#' Delete the minimal value in a heap
#' @param heap The heap
#' @return The heap with the minimal value removed
#' @export
delete_minimal <- function(heap) UseMethod("delete_minimal")



## Leftist heap ##############################

# helper function for creating nodes
leftist_heap_node <- function(
  value
  , left = empty_leftist_heap()
  , right = empty_leftist_heap()
  , rank = 0
  ) {
  structure(list(left = left, value = value, right = right, rank = 0),
            class = c("leftist_heap", "heap"))
}

# Dummy object used to represent an empty heap
empty_leftist_heap_node <- leftist_heap_node(NA, NULL, NULL)

#' Construct an empty leftist heap
#' @return an empty leftist heap
#' @export
empty_leftist_heap <- function() empty_leftist_heap_node

#' Test whether a leftist heap is empty
#' @param x Leftist heap
#' @return Whether the heap is empty
#' @method is_empty leftist_heap
#' @export
is_empty.leftist_heap <- function(x)
  identical(x, empty_leftist_heap_node)

#' @method find_minimal leftist_heap
#' @export
find_minimal.leftist_heap <- function(heap) {
  if (is_empty(heap)) stop("Can't get the minimal value in an empty heap")
  heap$value
}

#' @method delete_minimal leftist_heap
#' @export
delete_minimal.leftist_heap <- function(heap) {
  if (is_empty(heap)) stop("Can't delete the minimal value in an empty heap")
  merge(heap$left, heap$right)
}

# helper function for constructing leftist heaps
build_leftist_heap <- function(value, a, b) {
  if (a$rank >= b$rank)
    leftist_heap_node(value = value, left = a, right = b, rank = b$rank + 1)
  else
    leftist_heap_node(value = value, left = b, right = a, rank = a$rank + 1)
}

#' @method merge leftist_heap
#' @export
merge.leftist_heap <- function(x, y, ...) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)
  if (x$value <= y$value) build_leftist_heap(x$value, x$left, merge(x$right, y))
  else build_leftist_heap(y$value, y$left, merge(x, y$right))
}

#' @method insert leftist_heap
#' @export
insert.leftist_heap <- function(x, elm, ...) {
  merge(x, leftist_heap_node(elm))
}
