# This file contains some basic data structures that the
# abstract data structures elsewhere are based upon, plus some generic functions
# used by all data structures

#' Test if a data structure is empty
#' @param x The data structure
#' @return TRUE if x is empty.
#' @export
is_empty <- function(x) UseMethod("is_empty")

#' Insert a value into a data structure.
#' @param x The data structure
#' @param elm The element
#' @param ... Potential optional arguments
#' @return The updated data structure
#' @export
insert <- function(x, elm, ...) UseMethod("insert")

#' Merge two data structures (of the same kind)
#' @param x The first data structure
#' @param y The second data structure
#' @param ... Potential optional arguments
#' @return the merged x and y
#' @export
merge <- function(x, y, ...) UseMethod("merge")

## Linked lists #########################

#' Create an empty linked list.
#' @return an empty linked list.
#' @export
empty_list <- function() NULL

#' Add a head item to a linked list.
#' @param elem  The item to put at the head of the list.
#' @param lst   The list -- it will become the tail of the new list.
#' @return a new linked list.
#' @export
list_cons <- function(elem, lst)
  structure(list(item = elem, tail = lst), class = "linked_list")

#' Get the item at the head of a linked list.
#' @param lst The list
#' @return The element at the head of the list.
#' @export
list_head <- function(lst) lst$item

#' Get the tail of a linked list.
#' @param lst The list
#' @return The tail of the list
#' @export
list_tail <- function(lst) lst$tail

#' Compute the length of a list.
#'
#' This is a linear time algorithm.
#'
#' @param lst The lsit
#' @return The number of elements in the list.
#' @export
list_length <- function(lst) {
  n <- 0
  while (!is.null(lst)) {
    lst <- lst$tail
    n <- n + 1
  }
  n
}

#' Translate a linked list into an R list.
#'
#' @param x The linked list
#' @param ... Additional parameters. Not used but part of the generic interface.
#' @return An R list with the elements found in the linked list.
#' @method as.list linked_list
#' @export
as.list.linked_list <- function(x, ...) {
  n <- list_length(x)
  result <- vector(mode = "list", length = n)
  i <- 1
  while (!is.null(x)) {
    result[i] <- x$item
    x <- x$tail
    i <- i + 1
  }
  result
}

#' Translate a linked list into an R vector.
#'
#' @param x    The linked list
#' @param mode The mode (numeric, logical, etc.) for the constructed vector.
#'             The default will be a list
#' @return An R vector with the elements found in the linked list.
#' @method as.vector linked_list
#' @export
as.vector.linked_list <- function(x, mode = "any") {
  as.vector(as.list(x), mode)
}

