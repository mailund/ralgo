## Generic functions for search trees ###############

st_member <- function(x, elm, candidate = NA) {
  if (is_empty(x)) return(!is.na(candidate) && elm == candidate)
  if (elm < x$value) st_member(x$left, elm, candidate)
  else st_member(x$right, elm, x$value)
}

#' @method member search_tree
#' @export
member.search_tree <- function(x, elm, ...) {
  return(st_member(x, elm))
}

st_leftmost <- function(tree) {
  while (!is_empty(tree)) {
    value <- tree$value
    tree <- tree$left
  }
  value
}



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
extract_graph.search_tree <- function(tree) {

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
#' @method plot search_tree
#' @export
plot.search_tree <- function(x, ...) {
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
    geom_node_text(aes(label = value), vjust = 0.4) +
    theme_graph()
}



## unbalanced search tree ############################
search_tree_node <- function(
  value
  , left = empty_search_tree()
  , right = empty_search_tree()
) {
  structure(list(left = left, value = value, right = right),
            class = c("unbalanced_search_tree", "search_tree"))
}

# special node for empty trees
empty_search_tree_node = search_tree_node(NA, NULL, NULL)

#' Create empty unbalanced search tree
#' @return New empty, unbalanced, search tree
#' @export
empty_search_tree <- function() empty_search_tree_node

#' @method is_empty unbalanced_search_tree
#' @export
is_empty.unbalanced_search_tree <- function(x)
  is.null(x$left) && is.null(x$right)


st_insert <- function(tree, elm) {
  if (is_empty(tree)) return(search_tree_node(elm))
  if (elm < tree$value)
    search_tree_node(tree$value, st_insert(tree$left, elm), tree$right)
  else if (elm > tree$value)
    search_tree_node(tree$value, tree$left, st_insert(tree$right, elm))
  else
    tree # the value is already in the tree, at this level, so just return
}

#' @method insert unbalanced_search_tree
#' @export
insert.unbalanced_search_tree <- function(x, elm, ...) {
  st_insert(x, elm)
}

st_remove <- function(tree, elm) {
  # if we reach an empty tree, there is nothing to do
  if (is_empty(tree)) return(tree)

  if (tree$value == elm) {
    a <- tree$left
    b <- tree$right
    if (is_empty(a)) return(b)
    if (is_empty(b)) return(a)

    s <- st_leftmost(tree$right)
    return(search_tree_node(s, a, st_remove(b, s)))
  }

  # we need to search further down to remove the element
  if (elm < tree$value)
    search_tree_node(tree$value, st_remove(tree$left, elm), tree$right)
  else # (elm > tree$value)
    search_tree_node(tree$value, tree$left, st_remove(tree$right, elm))
}

#' @method remove unbalanced_search_tree
#' @export
remove.unbalanced_search_tree <- function(x, elm, ...) {
  st_remove(x, elm)
}


## Red-Black search tree ############################

# colours
RED <- 1
BLACK <- 2
DOUBLE_BLACK <- 3

# helper function
red_black_tree_node <- function(
  colour
  , value
  , left = empty_red_black_tree()
  , right = empty_red_black_tree()
  ) {
  structure(list(colour = colour, left = left, value = value, right = right),
            class = c("red_black_tree", "search_tree"))
}

# special node for empty trees
empty_red_black_tree_node <- red_black_tree_node(BLACK, NA, NULL, NULL)

#' Create empty red-black search tree
#' @return New, empty, red-black search tree
#' @export
empty_red_black_tree <- function() empty_red_black_tree_node

#' @method is_empty red_black_tree
#' @export
is_empty.red_black_tree <- function(x)
  is.null(x$left) && is.null(x$right)


rbt_balance <- function(colour, value, left, right) {
  a <- b <- c <- d <- x <- y <- z <- NULL # Setting these to avoid warnings
  if (pattern_match(a = left$left, b = left$right$left,
                    c = left$right$right, d = right,
                    x = left$value, y = left$right$value, z = value,
                    colour == BLACK, left$colour == RED, left$right$colour == RED)

      || pattern_match(a = left$left$left, b = left$left$right,
                       c = left$right, d = right,
                       x = left$left$value, y = left$value, z = value,
                       colour == BLACK, left$colour == RED, left$left$colour == RED)

      || pattern_match(a = left, b = right$left,
                       c = right$right$left, d = right$right$right,
                       x = value, y = right$value, z = right$right$value,
                       colour == BLACK, right$colour == RED, right$right$colour == RED)

      || pattern_match(a = left, b = right$left$left,
                       c = right$left$right, d = right$right,
                       x = value, y = right$left$value, z = right$value,
                       colour == BLACK, right$colour == RED, right$left$colour == RED)
  ) {

    left <- red_black_tree_node(colour = BLACK, value = x, left = a, right = b)
    right <- red_black_tree_node(colour = BLACK, value = z, left = c, right = d)
    red_black_tree_node(colour = RED, value = y, left, right)

  } else if (pattern_match(a = left$left,
                           b = left$right$left,
                           c = left$right$right,
                           d = right,
                           z = value,
                           x = left$value,
                           y = left$right$value,
                           colour == DOUBLE_BLACK,
                           left$colour == RED,
                           left$right$colour == RED)
             || pattern_match(a = left,
                              b = right$left$left,
                              c = right$left$right,
                              d = right$right,
                              x = value,
                              z = right$value,
                              y = right$left$value,
                              colour == DOUBLE_BLACK,
                              right$colour == RED,
                              right$left$colour == RED)) {

    left <- red_black_tree_node(BLACK, x, a, b)
    right <- red_black_tree_node(BLACK, z, c, d)
    red_black_tree_node(BLACK, y, left, right)

  } else {
    red_black_tree_node(colour, value, left, right)
  }
}

rbt_insert <- function(tree, elm) {
  if (is_empty(tree)) return(red_black_tree_node(RED, elm))
  if (elm < tree$value)
    rbt_balance(tree$colour, tree$value, rbt_insert(tree$left, elm), tree$right)
  else if (elm > tree$value)
    rbt_balance(tree$colour, tree$value, tree$left, rbt_insert(tree$right, elm))
  else
    tree # the value is already in the tree, at this level, so just return
}

#' @method insert red_black_tree
#' @export
insert.red_black_tree <- function(x, elm, ...) {
  # insert the value in the tree and set the root to be black
  new_tree <- rbt_insert(x, elm)
  new_tree$colour <- BLACK
  new_tree
}

#' @method member red_black_tree
#' @export
member.red_black_tree <- function(x, elm, ...) {
  if (is_empty(x)) return(FALSE)
  if (x$value == elm) return(TRUE)

  if (elm < x$value) member(x$left, elm)
  else member(x$right, elm)
}

rbt_rotate <- function(colour, value, left, right) {
  a <- b <- c <- d <- e <- x <- y <- z <- NULL # Setting these to avoid warnings

  # first case
  if (pattern_match(a_x_b = left, c = right$left, d = right$right,
                    y = value, z = right$value,
                    a_x_b$colour == DOUBLE_BLACK,
                    colour == RED,
                    right$colour == BLACK)) {

    a_x_b$colour <- BLACK
    rbt_balance(BLACK, z,
                red_black_tree_node(RED, y, a_x_b, c),
                d)

  } else if (pattern_match(a = left$left, b = left$right,
                           c_z_d = right,
                           y = value, x = left$value,
                           left$colour == BLACK,
                           colour == RED,
                           c_z_d$colour == DOUBLE_BLACK)) {

    c_z_d$colour <- BLACK
    rbt_balance(BLACK, x,
                a,
                red_black_tree_node(RED, y, b, c_z_d))


  # second case
  } else if (pattern_match(a_x_b = left, c = right$left, d = right$right,
                           y = value, z = right$value,
                           colour == BLACK,
                           a_x_b$colour == DOUBLE_BLACK,
                           right$colour == BLACK)) {

    new_left <- red_black_tree_node(RED, y, a_x_b, c)
    rbt_balance(DOUBLE_BLACK, z, new_left, d)

  } else if (pattern_match(a = left$left, b = left$right,
                           y = value, c_z_d = right,
                           left$colour == BLACK,
                           colour == BLACK,
                           c_z_d$colour == DOUBLE_BLACK)) {

    new_right <- red_black_tree_node(RED, y, b, c_z_d)
    rbt_balance(DOUBLE_BLACK, x, a, new_right)

  # third case
  } else if (pattern_match(a_w_b = left,
                           c = right$left$left,
                           d = right$left$right,
                           e = right$right,
                           x = value, z = right$value,
                           y = right$left$value,
                           a_w_b$colour == DOUBLE_BLACK,
                           colour == BLACK,
                           right$colour == RED)) {

    a_w_b$colour <- BLACK
    new_left_left <- red_black_tree_node(RED, x, a_w_b, c)
    new_left <- rbt_balance(BLACK, y, new_left_left, d)
    red_black_tree_node(BLACK, z, new_left, e)

  } else if (pattern_match(a = left$left,
                           b = left$right$left,
                           c = left$right$right,
                           d_w_e = right,
                           z = left$right$value,
                           x = left$value,
                           y = value,
                           left$colour == RED,
                           colour == BLACK,
                           d_w_e$colour == DOUBLE_BLACK)) {

    d_w_e$colour <- BLACK
    new_right_right <- red_black_tree_node(RED, y, c, d_w_e)
    new_right <- rbt_balance(BLACK, z, b, new_right_right)
    red_black_tree_node(BLACK, x, a, new_right)

  } else {
    red_black_tree_node(colour, value, left, right)
  }
}

rbt_remove <- function(tree, elm) {
  if (is_empty(tree)) { # we didn't find the value...
    return(tree)
  }

  if (tree$value == elm) { # found the value to delete
    a <- tree$left
    b <- tree$right
    if (is_empty(a) && is_empty(b)) { # leaf
      if (tree$colour == BLACK)
        return(red_black_tree_node(DOUBLE_BLACK, NA, NULL, NULL))
      else
        return(red_black_tree_node(RED, NA, NULL, NULL))

    } else if (is_empty(a) || is_empty(b)) { # one empty child
      non_empty <- if (is_empty(a)) b else a
      non_empty$colour <- BLACK
      return(non_empty)

    } else { # inner node
      s <- st_leftmost(tree$right)
      return(rbt_rotate(tree$colour, s, a, rbt_remove(b, s)))
    }
  }

  # we need to search further down to remove the element
  if (elm < tree$value)
    rbt_rotate(tree$colour, tree$value, rbt_remove(tree$left, elm), tree$right)
  else # (elm > tree$value)
    rbt_rotate(tree$colour, tree$value, tree$left, rbt_remove(tree$right, elm))
}

#' @method remove red_black_tree
#' @export
remove.red_black_tree <- function(x, elm, ...) {
  rbt_remove(x, elm)
}


#' @import tibble
extract_graph.red_black_tree <- function(tree) {
  n <- tree$dfn
  colours <- vector("numeric", length = n)
  extract <- function(tree) {
    # we change the index so the root is number 1 -- that is easier
    i <- n - tree$dfn + 1
    colours[i] <<- tree$colour
  }
  depth_first_visit_binary_tree(tree, extract)

  graph <- NextMethod()
  RB <- c("Red", "Black", "Double black")
  nodes <- graph$nodes %>% add_column(colour = RB[colours])
  edges <- graph$edges
  list(nodes = nodes, edges = edges)
}

#' @import ggraph
#' @import ggplot2
#' @method plot red_black_tree
#' @export
plot.red_black_tree <- function(x, ...) {
  NextMethod() +
    scale_fill_manual("Colour",
                      values = c("Red" = "white",
                                 "Black" = "black",
                                 "Double black" = "lightgray")) +
    geom_node_point(aes_(filter = quote(leaf), fill = quote(colour)), size = 2, shape = 21) +
    geom_node_point(aes_(filter = quote(!leaf), fill = quote(colour)), size = 10, shape = 21) +
    geom_node_text(aes_(filter = quote(colour == "Black"), label = quote(value)),
                   colour = 'white', vjust = 0.4) +
    geom_node_text(aes_(filter = quote(colour == "Double black"), label = quote(value)),
                   colour = 'black', vjust = 0.4) +
    geom_node_text(aes_(filter = quote(colour == "Red"), label = quote(value)),
                   colour = 'black', vjust = 0.4)
}

