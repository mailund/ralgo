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
            class = c("leftist_heap", "heap", "binary_tree"))
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
  is.null(x$left) && is.null(x$right)

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

#' @method size leftist_heap
#' @export
size.leftist_heap <- function(x) {
  f <- function(x) {
    if (is_empty(x)) 0
    else 1 + f(x$left) + f(x$right)
  }
  f(x)
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


#' @method size binomial_heap
#' @export
size.binomial_heap <- function(x) {
  tree_size <- function(x) {
    1 + tree_list_size(x$trees)
  }
  tree_list_size <- function(x) {
    if (is_empty(x)) 0
    else tree_size(list_head(x)) + tree_list_size(list_tail(x))
  }
  heap_size <- function(x) {
    if (is_empty(x)) 0
    else tree_size(list_head(x)$tree) + heap_size(list_tail(x))
  }
  heap_size(x$heap_nodes)
}


# plotting code -- we don't test that
# nocov start

number_binomial_trees <- function(trees, n) {
  if (is_empty(trees)) {
    empty_list()
  } else {
    this <- list_head(trees)
    rest <- number_binomial_trees(list_tail(trees), n)
    new_n <- ifelse(is_empty(rest), n, list_head(rest)$number)
    new_subtrees <- number_binomial_trees(this$trees, new_n)
    new_n <- ifelse(is_empty(new_subtrees), new_n, list_head(new_subtrees)$number)
    new_tree <- list(number = new_n + 1, value = this$value, trees = new_subtrees)
    list_cons(new_tree, rest)
  }
}

number_binomial_heap_nodes <- function(nodes, n = 0) {
  if (is_empty(nodes)) {
    empty_list()
  } else {
    this <- list_head(nodes)
    rest <- number_binomial_heap_nodes(list_tail(nodes), n)
    new_n <- ifelse(is_empty(rest), n, list_head(rest)$number)

    # unfortunately special case with a single tree...
    tree <- this$tree
    subtrees <- number_binomial_trees(tree$trees, new_n)
    new_n <- ifelse(is_empty(subtrees), new_n, list_head(subtrees)$number)
    new_tree <- list(number = new_n + 1, value = tree$value, trees = subtrees)

    new_node <- list(number = new_n + 2, rank = this$rank, tree = new_tree)
    list_cons(new_node, rest)
  }
}

number_binomial_heap <- function(heap) {
  heap_nodes <- number_binomial_heap_nodes(heap$heap_nodes, 0)
  heap_nodes
}

extract_binomial_heap_nodes_graph <- function(nodes) {
  n <- list_head(nodes)$number
  values <- vector("numeric", length = n)
  heap_nodes <- vector("logical", length = n)
  from <- vector("integer", length = n - 1)
  to <- vector("integer", length = n - 1)
  edge_idx <- 1

  extract_tree <- function(tree, parent) {
    values[tree$number] <<- tree$value
    heap_nodes[tree$number] <<- FALSE
    from[edge_idx] <<- parent
    to[edge_idx] <<- tree$number
    edge_idx <<- edge_idx + 1
    extract_trees(tree$trees, tree$number)
  }

  extract_trees <- function(trees, parent) {
    if (!is_empty(trees)) {
      extract_tree(list_head(trees), parent)
      extract_trees(list_tail(trees), parent)
    }
  }

  extract <- function(nodes) {
    if (!is_empty(nodes)) {
      node <- list_head(nodes)
      values[node$number] <<- node$rank
      heap_nodes[node$number] <<- TRUE

      if (!is_empty(list_tail(nodes))) {
        next_node <- list_head(list_tail(nodes))
        from[edge_idx] <<- node$number
        to[edge_idx] <<- next_node$number
        edge_idx <<- edge_idx + 1
      }

      extract_tree(node$tree, node$number)
      extract(list_tail(nodes))
    }
  }

  extract(nodes)

  list(nodes = tibble(heap_node = heap_nodes, value = values),
       edges = tibble(from = from, to = to))
}

#' @method plot binomial_heap
#' @export
plot.binomial_heap <- function(x, ...) {
  info <- x %>%
    number_binomial_heap %>%
    extract_binomial_heap_nodes_graph

  tbl_graph(info$nodes, info$edges) %>%
    mutate(leaf = node_is_leaf()) %>%
    ggraph(layout = "tree") +
    scale_x_reverse() +
    geom_edge_link() +
    geom_node_point(aes_(filter = quote(heap_node)),
                    size = 10, shape = 21, fill = "black") +
    geom_node_text(aes_(filter = quote(heap_node),
                        label = quote(value)),
                   vjust = 0.4, color = "white") +
    geom_node_point(aes_(filter = quote(!heap_node)),
                    size = 10, shape = 21, fill = "white") +
    geom_node_text(aes_(filter = quote(!heap_node),
                        label = quote(value)),
                   vjust = 0.4, color = "black") +
    theme_graph()

}

# nocov end

## Splay heap ##############################

# helper function for creating nodes
splay_tree_node <- function(value, left = NULL, right = NULL) {
  structure(list(left = left, value = value, right = right),
            class = c("splay_node", "binary_tree"))
}

empty_splay_node <- function()
  splay_tree_node(NA)

#' @method is_empty splay_node
#' @export
is_empty.splay_node <- function(x)
  is.na(x$value) && is.null(x$left) && is.null(x$right)

splay_heap <- function(min_value, splay_tree) {
  structure(list(min_value = min_value, tree = splay_tree),
            class = c("splay_heap", "heap"))
}

#' Construct an empty splay heap
#' @return an empty splay heap
#' @export
empty_splay_heap <- function() splay_heap(NA, empty_splay_node())

#' Test whether a splay heap is empty
#' @param x splay heap
#' @return Whether the heap is empty
#' @method is_empty splay_heap
#' @export
is_empty.splay_heap <- function(x) is_empty(x$tree)

#' @method find_minimal splay_heap
#' @export
find_minimal.splay_heap <- function(heap) {
  heap$min_value
}

splay_find_minimal_value <- function(tree) {
  if (is_empty(tree)) NA
  else if (is_empty(tree$left)) tree$value
  else splay_find_minimal_value(tree$left)
}

splay_delete_minimal_value <- function(tree) {
  if (is_empty(tree$left)) {
    tree$right

  } else {
    a <- tree$left$left
    x <- tree$left$value
    b <- tree$left$right
    y <- tree$value
    c <- tree$right

    if (is_empty(a))
      splay_tree_node(left = b, value = y, right = c)
    else
      splay_tree_node(
        left = splay_delete_minimal_value(a),
        value = x,
        right = splay_tree_node(left = b, value = y, right = c)
      )
  }
}


#' @method delete_minimal splay_heap
#' @export
delete_minimal.splay_heap <- function(heap) {
  if (is_empty(heap)) stop("Can't delete the minimal value in an empty heap")
  new_tree <- splay_delete_minimal_value(heap$tree)
  new_min_value <- splay_find_minimal_value(new_tree)
  splay_heap(min_value = new_min_value, splay_tree = new_tree)
}

is_case_1 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  x <= pivot && is_empty(b)
}

transform_case_1 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  list(smaller = tree, larger = empty_splay_node())
}

is_case_2 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right
  x <= pivot && y <= pivot
}

transform_case_2 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right

  part <- partition(pivot, b2)
  smaller <- splay_tree_node(
    left = splay_tree_node(
      left = a,
      value = x,
      right = b1
    ),
    value = y,
    right = part$smaller
  )
  larger <- part$larger
  list(smaller = smaller, larger = larger)
}

is_case_3 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right
  x <= pivot && y > pivot
}

transform_case_3 <- function(pivot, tree) {
  # is only called when right is not empty...
  a <- tree$left
  x <- tree$value
  b1 <- tree$right$left
  y <- tree$right$value
  b2 <- tree$right$right
  part <- partition(pivot, b1)
  smaller <- splay_tree_node(
    left = a,
    value = x,
    right = part$smaller
  )
  larger <- splay_tree_node(
    left = part$larger,
    value = y,
    right = b2
  )

  list(smaller = smaller, larger = larger)
}

is_case_4 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  x > pivot && is_empty(a)
}

transform_case_4 <- function(pivot, tree) {
  a <- tree$left
  x <- tree$value
  b <- tree$right
  list(smaller = empty_splay_node(), larger = tree)
}

is_case_5 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right
  x > pivot && y <= pivot
}

transform_case_5 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right
  part <- partition(pivot, a2)
  smaller <- splay_tree_node(
    left = a1,
    value = y,
    right = part$smaller
  )
  larger <- splay_tree_node(
    left = part$larger,
    value = x,
    right = b
  )

  list(smaller = smaller, larger = larger)
}

is_case_6 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right
  x > pivot && y > pivot
}

transform_case_6 <- function(pivot, tree) {
  # is only called when left is not empty
  a1 <- tree$left$left
  y <- tree$left$value
  a2 <- tree$left$right
  x <- tree$value
  b <- tree$right
  part <- partition(pivot, a1)
  smaller <- part$smaller
  larger <- splay_tree_node(
    left = part$larger,
    value = y,
    right = splay_tree_node(
      left = a2,
      value = x,
      right = b
    )
  )
  list(smaller = smaller, larger = larger)
}

partition <- function(pivot, tree) {
  if (is_empty(tree))
    list(smaller = empty_splay_node(), larger = empty_splay_node())
  else if (is_case_1(pivot, tree))
    transform_case_1(pivot, tree)
  else if (is_case_2(pivot, tree))
    transform_case_2(pivot, tree)
  else if (is_case_3(pivot, tree))
    transform_case_3(pivot, tree)
  else if (is_case_4(pivot, tree))
    transform_case_4(pivot, tree)
  else if (is_case_5(pivot, tree))
    transform_case_5(pivot, tree)
  else if (is_case_6(pivot, tree))
    transform_case_6(pivot, tree)
  else stop("Unknown case")
}

#' @method insert splay_heap
#' @export
insert.splay_heap <- function(x, elm, ...) {
  part <- partition(elm, x$tree)
  new_tree <- splay_tree_node(
    value = elm,
    left = part$smaller,
    right = part$larger
  )
  new_min_value <- min(x$min_value, elm, na.rm = TRUE)
  splay_heap(min_value = new_min_value, splay_tree = new_tree)
}

merge_splay_trees <- function(x, y) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)

  a <- x$left
  val <- x$value
  b <- x$right

  part <- partition(val, y)
  splay_tree_node(left = merge_splay_trees(part$smaller, a),
                  value = val,
                  right = merge_splay_trees(part$larger, b))
}

#' @method merge splay_heap
#' @export
merge.splay_heap <- function(x, y, ...) {
  if (is_empty(x)) return(y)
  if (is_empty(y)) return(x)

  new_tree <- merge_splay_trees(x$tree, y$tree)
  new_min_value <- min(x$min_value, y$min_value, na.rm = TRUE)
  splay_heap(min_value = new_min_value, splay_tree = new_tree)
}

#' @method size splay_heap
#' @export
size.splay_heap <- function(x) {
  f <- function(x) {
    if (is_empty(x)) 0
    else 1 + f(x$left) + f(x$right)
  }
  f(x$tree)
}


# nocov start

#' @method plot splay_heap
#' @export
plot.splay_heap <- function(x, ...) {
  info <- x$tree %>%
    node_number_annotate_tree %>%
    extract_graph

  # modify indices to make a new root...
  nodes <- tibble(value = c(find_minimal(x), info$nodes$value),
                  type = c("Root", rep("Vertex", length(info$nodes$value))))
  edges <- tibble(from = c(1, info$edges$from + 1),
                  to = c(2, info$edges$to + 1))

  tbl_graph(nodes, edges) %>%
    mutate(leaf = node_is_leaf()) %>%
    ggraph(layout = "tree") +
    scale_x_reverse() +
    geom_edge_link() +
    geom_node_point(aes_(filter = quote(leaf)), size = 2, shape = 21, fill = "black") +
    geom_node_point(aes_(filter = quote(type == "Vertex" & !leaf)), size = 10, shape = 21, fill = "white") +
    geom_node_point(aes_(filter = quote(type == "Root")), size = 10, shape = 19, fill = "black") +
    geom_node_text(aes_(label = quote(value)), vjust = 0.4) +
    geom_node_text(aes_(filter = quote(type == "Root"), label = quote(value)), vjust = 0.4, color = "white") +
    theme_graph()
}

# nocov end
