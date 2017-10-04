# nocov start

node_number_annotate_tree <- function(tree, i = 1) {
  if (is_empty(tree)) {
    tree$dfn <- i
  } else {
    left <- node_number_annotate_tree(tree$left, i)
    right <- node_number_annotate_tree(tree$right, left$dfn + 1)
    tree$dfn <- right$dfn + 1
    tree$left <- left
    tree$right <- right
  }
  tree
}

depth_first_visit_binary_tree <- function(tree, visitor) {
  if (!is_empty(tree)) {
    depth_first_visit_binary_tree(tree$left, visitor)
    depth_first_visit_binary_tree(tree$right, visitor)
  }
  visitor(tree)
}

# function for extracting a graph from a tree
extract_graph <- function(tree) UseMethod("extract_graph")

#' @import tibble
extract_graph.binary_tree <- function(tree) {

  n <- tree$dfn
  values <- vector("numeric", length = n)
  from <- vector("integer", length = n - 1)
  to <- vector("integer", length = n - 1)
  edge_idx <- 1

  extract <- function(tree) {
    # we change the index so the root is number 1 -- that is easier
    i <- n - tree$dfn + 1
    values[i] <<- ifelse(is.na(tree$value), "", tree$value)

    if (!is_empty(tree)) {
      j <- n - tree$left$dfn + 1
      from[edge_idx] <<- i
      to[edge_idx] <<- j
      edge_idx <<- edge_idx + 1

      k <- n - tree$right$dfn + 1
      from[edge_idx] <<- i
      to[edge_idx] <<- k
      edge_idx <<- edge_idx + 1
    }
  }

  depth_first_visit_binary_tree(tree, extract)
  nodes <- tibble(value = values)
  edges <- tibble(from = from, to = to)
  list(nodes = nodes, edges = edges)
}


#' @import ggraph
#' @import tidygraph
#' @method plot binary_tree
#' @export
plot.binary_tree <- function(x, ...) {
  info <- x %>%
    node_number_annotate_tree %>%
    extract_graph

  tbl_graph(info$nodes, info$edges) %>%
    mutate(leaf = node_is_leaf()) %>%
    ggraph(layout = "tree") +
    scale_x_reverse() +
    geom_edge_link() +
    geom_node_point(aes_(filter = quote(leaf)), size = 2, shape = 21, fill = "black") +
    geom_node_point(aes_(filter = quote(!leaf)), size = 10, shape = 21, fill = "white") +
    geom_node_text(aes_(label = quote(value)), vjust = 0.4) +
    theme_graph()
}

# nocov end