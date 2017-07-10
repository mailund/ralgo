## Generic functions for search trees ###############

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
is_empty.unbalanced_search_tree <- function(x) identical(x, empty_search_tree_node)


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

st_member <- function(x, elm, candidate = NA) {
  if (is_empty(x)) return(!is.na(candidate) && elm == candidate)
  if (elm < x$value) st_member(x$left, elm, candidate)
  else st_member(x$right, elm, x$value)
}

#' @method member unbalanced_search_tree
#' @export
member.unbalanced_search_tree <- function(x, elm, ...) {
  return(st_member(x, elm))
  if (is_empty(x)) return(FALSE)
  if (x$value == elm) return(TRUE)
  if (elm < x$value) member(x$left, elm)
  else member(x$right, elm)
}

st_leftmost <- function(tree) {
  while (!is_empty(tree)) {
    value <- tree$value
    tree <- tree$left
  }
  value
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
RED <- 0
BLACK <- 1
DOUBLE_BLACK <- 2

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
empty_red_black_tree_node <- red_black_tree_node(RED, NA, NULL, NULL)

#' Create empty red-black search tree
#' @return New, empty, red-black search tree
#' @export
empty_red_black_tree <- function() empty_red_black_tree_node

#' @method is_empty red_black_tree
#' @export
is_empty.red_black_tree <- function(x) identical(x, empty_red_black_tree_node)


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

rbt_remove <- function(tree, elm) { # FIXME: rebalancing not done correctly yet
  # if we reach an empty tree, there is nothing to do
  if (is_empty(tree)) return(tree)

  if (tree$value == elm) {
    a <- tree$left
    b <- tree$right
    if (is_empty(a)) return(b)
    if (is_empty(b)) return(a)

    s <- st_leftmost(tree$right)
    return(rbt_balance(tree$colour, s, a, rbt_remove(b, s)))
  }

  # we need to search further down to remove the element
  if (elm < tree$value)
    rbt_balance(tree$colour, tree$value, rbt_remove(tree$left, elm), tree$right)
  else # (elm > tree$value)
    rbt_balance(tree$colour, tree$value, tree$left, rbt_remove(tree$right, elm))
}

#' @method remove red_black_tree
#' @export
remove.red_black_tree <- function(x, elm, ...) {
  new_tree <- rbt_remove(x, elm)
  if (!is_empty(new_tree)) new_tree$colour <- BLACK
  new_tree
}
