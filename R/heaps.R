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

## Functions working on heaps ################

singleton_heap <- function(empty_heap, e) insert(empty_heap, e)

#' Construct a heap from a vector of elements.
#'
#' This function builds heaps in linear time by iteratively merging larger and larger heaps.
#'
#' @param vec The vector to translate into a heap.
#' @param empty_heap Empty heap for determing which heap implementation to use.
#' @param empty_queue Empty queue to determing which queue implementation to use for
#'                    building the heap.
#' @export
vector_to_heap <- function(vec, empty_heap, empty_queue = empty_env_queue()) {
  q <- empty_queue
  for (e in vec)
    q <- enqueue(q, singleton_heap(empty_heap, e))
  repeat {
    first <- front(q) ; q <- dequeue(q)
    if (is_empty(q)) break
    second <- front(q) ; q <- dequeue(q)
    new_heap <- merge(first, second)
    q <- enqueue(q, new_heap)
  }
  first
}

## Leftist heap ##############################

# helper function for creating nodes
leftist_heap_node <- function(
  value
  , left = empty_leftist_heap()
  , right = empty_leftist_heap()
  , rank = 0
  ) {
  structure(list(left = left, value = value, right = right, rank = rank),
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


## Binomial heap ##############################

binomial_tree_node <- function(value, trees) {
  list(value = value, trees = trees)
}

link_binomial_trees <- function(t1, t2) {
  if (t1$value < t2$value) {
    binomial_tree_node(t1$value, list_cons(t2, t1$trees))
  } else {
    binomial_tree_node(t2$value, list_cons(t1, t2$trees))
  }
}

binomial_heap_node <- function(rank, tree) {
  list(rank = rank, tree = tree)
}

singleton_binomial_heap_node <- function(value) {
  tree <- binomial_tree_node(value, empty_list())
  binomial_heap_node(0, tree)
}

binomial_heap <- function(min_value, heap_nodes = empty_list()) {
  structure(list(min_value = min_value, heap_nodes = heap_nodes),
            class = c("binomial_heap", "heap"))
}

#' Construct an empty binomial heap
#' @return an empty binomial heap
#' @export
empty_binomial_heap <- function() binomial_heap(NA)

#' Test whether a binomial heap is empty
#' @param x binomial heap
#' @return Whether the heap is empty
#' @method is_empty binomial_heap
#' @export
is_empty.binomial_heap <- function(x) is_empty(x$heap_nodes)

#' @method find_minimal binomial_heap
#' @export
find_minimal.binomial_heap <- function(heap) {
  heap$min_value
}


#' @method insert binomial_heap
#' @export
insert.binomial_heap <- function(x, elm, ...) {
  new_min_value <- min(x$min_value, elm, na.rm = TRUE)
  new_node <- singleton_binomial_heap_node(elm)
  new_nodes <- insert_binomial_node(new_node, x$heap_nodes)
  binomial_heap(new_min_value, new_nodes)
}

# merging two lists of heap nodes work like binary addition...
merge_heap_nodes <- function(x, y) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)

  first_x <- list_head(x)
  first_y <- list_head(y)
  if (first_x$rank < first_y$rank) {
    list_cons(first_x, merge_heap_nodes(list_tail(x), y))
  } else if (first_y$rank < first_x$rank) {
    list_cons(first_y, merge_heap_nodes(list_tail(y), x))
  } else {
    new_tree <- link_binomial_trees(first_x$tree, first_y$tree)
    new_node <- binomial_heap_node(first_x$rank + 1, new_tree)
    rest <- merge_heap_nodes(list_tail(x), list_tail(y))
    insert_binomial_node(new_node, rest)
  }
}

insert_binomial_node <- function(new_node, heap_nodes) {
  return(merge_heap_nodes(list_cons(new_node, empty_list()), heap_nodes))
}

#' @method merge binomial_heap
#' @export
merge.binomial_heap <- function(x, y, ...) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)
  new_min_value <- min(x$min_value, y$min_value)
  new_nodes <- merge_heap_nodes(x$heap_nodes, y$heap_nodes)
  binomial_heap(new_min_value, new_nodes)
}

get_minimal_node <- function(min_value, heap_nodes) {
  # we should never reach an empty list since the min_value must be in there...
  first_node <- list_head(heap_nodes)
  if (first_node$tree$value == min_value) first_node
  else get_minimal_node(min_value, list_tail(heap_nodes))
}

delete_minimal_node <- function(min_value, heap_nodes) {
  # we should never reach an empty list since the min_value must be in there...
  first_node <- list_head(heap_nodes)
  if (first_node$tree$value == min_value) {
    list_tail(heap_nodes)
  } else {
    rest <- delete_minimal_node(min_value, list_tail(heap_nodes))
    list_cons(first_node, rest)
  }
}

binomial_trees_to_nodes <- function(rank, trees) {
  if (is_empty(trees)) {
    empty_list()
  } else {
    list_cons(binomial_heap_node(rank, list_head(trees)),
              binomial_trees_to_nodes(rank - 1, list_tail(trees)))
  }
}

binomial_nodes_min_value <- function(heap_nodes, cur_min = NA) {
  if (is_empty(heap_nodes)) {
    cur_min
  } else {
    front_value <- list_head(heap_nodes)$tree$value
    new_cur_min <- min(cur_min, front_value, na.rm = TRUE)
    binomial_nodes_min_value(list_tail(heap_nodes), new_cur_min)
  }
}

#' @method delete_minimal binomial_heap
#' @export
delete_minimal.binomial_heap <- function(heap) {
  if (is_empty(heap)) stop("Can't delete the minimal value in an empty heap")

  min_node <-
    get_minimal_node(heap$min_value, heap$heap_nodes)
  other_nodes <-
    delete_minimal_node(heap$min_value, heap$heap_nodes)
  min_node_nodes <-
    binomial_trees_to_nodes(min_node$rank - 1,
                            min_node$tree$trees)
  new_nodes <-
    merge_heap_nodes(other_nodes, list_reverse(min_node_nodes))
  new_min_value <- binomial_nodes_min_value(new_nodes)
  binomial_heap(new_min_value, new_nodes)
}

