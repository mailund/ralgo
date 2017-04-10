# Bag data structures

#' Add a head item to a linked list implementation of a bag.
#' @param elem  The item to put at the head of the bag
#' @param bag   The bag -- it will become the tail of the new list.
#' @return a new bag.
#' @export
bag_cons <- function(elem, lst)
  structure(list(item = elem, tail = lst), class = c("list_bag", "linked_list"))

bag_nil <- bag_cons(NA, NULL)

#' @method is_empty list_bag
#' @export
is_empty.list_bag <- function(x) identical(x, bag_nil)

#' Create an empty list bag.
#' @return an empty lsit bag.
#' @export
empty_list_bag <- function() bag_nil

#' @method insert list_bag
#' @export
insert.list_bag <- function(x, elm, ...) bag_cons(elm, x)

#' @method merge list_bag
#' @export
merge.list_bag <- function(x, y) {
  result <- list_concatenate(x, y)
  class(result) <- c("list_bag", "linked_list")
  result
}




#' Add a head item to a tree implementation of a bag.
#' @param elem  The item to put at the head of the bag
#' @param bag   The bag -- it will become the tail of the new list.
#' @return a new bag.
#' @export
bag_node <- function(elem, left, right)
  structure(list(item = elem, left = left, right = right),
            class = "tree_bag")

tree_bag_nil <- bag_node(NA, NULL, NULL)

#' @method is_empty tree_bag
#' @export
is_empty.tree_bag <- function(x) identical(x, tree_bag_nil)

#' Create an empty tree bag.
#' @return an empty tree bag.
#' @export
empty_tree_bag <- function() tree_bag_nil

#' @method insert tree_bag
#' @export
insert.tree_bag <- function(x, elm, ...) {
  element_leaf <- bag_node(elm, empty_tree_bag(), empty_tree_bag())
  if (is_empty(x)) element_leaf
  else bag_node(NA, element_leaf, x)
}

#' @method merge tree_bag
#' @export
merge.tree_bag <- function(x, y) {
  bag_node(NA, x, y)
}

is_leaf <- function(x) {
  is_empty(x$left) && is_empty(x$right)
}
bag_to_list <- function(x, acc) {
  if (is_leaf(x)) list_cons(x$item, acc)
  else bag_to_list(x$right, bag_to_list(x$left, acc))
}

#' Translate a tree bag into an R list.
#'
#' @param x The tree bag
#' @param ... Additional parameters. Not used but part of the generic interface.
#' @return An R list with the elements found in the linked list.
#' @method as.list tree_bag
#' @export
as.list.tree_bag <- function(x, ...) {
  as.list(bag_to_list(x, empty_list()))
}

#' Translate a tree bag into an R vector.
#'
#' @param x    The tree bag.
#' @param mode The mode (numeric, logical, etc.) for the constructed vector.
#'             The default will be a list
#' @return An R vector with the elements found in the bag.
#' @method as.vector tree_bag
#' @export
as.vector.tree_bag <- function(x, mode = "any") {
  as.vector(as.list(x), mode)
}