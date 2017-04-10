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

