## Generic functions for search trees ###############

## Red-Black search tree ############################

# colours
RED = 0
BLACK = 0

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
empty_red_black_tree_node = red_black_tree_node(BLACK, NA, NULL, NULL)

#' Create empty red-black search tree
#' @return New, empty, red-black search tree
#' @export
empty_red_black_tree <- function() empty_red_black_tree_node

#' Check if a red-black search tree is empty
#' @method is_empty red_black_tree
#' @export
is_empty.red_black_tree <- function(x) identical(x, empty_red_black_tree_node)


rbt_balance <- function(colour, value, left, right) {
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
      )
      return(red_black_tree_node(colour = RED, value = y,
                                 left = red_black_tree_node(colour = BLACK, value = x,
                                                            left = a, right = b),
                                 right = red_black_tree_node(colour = BLACK, value = z,
                                                             left = c, right = d)))

  red_black_tree_node(colour = colour, value = value,
                      left = left, right = right)
}
rbt_insert <- function(tree, elm) {
  if (is_empty(tree)) return(red_black_tree_node(RED, elm))
  if (elm < tree$value)
    rbt_balance(tree$colour, tree$value,
                rbt_insert(tree$left, elm), tree$right)
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
  new_tree$colour = BLACK
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

