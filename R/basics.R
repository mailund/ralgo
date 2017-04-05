# This file contains some basic data structures that the
# abstract data structures elsewhere are based upon.

#' Create an empty linked list.
#' @return an empty linked list.
#' @export
empty_list <- function() NULL

#' Add a head item to a linked list.
#' @param lst   The list -- it will become the tail of the new list.
#' @param elem  The item to put at the head of the list.
#' @return a new linked list.
#' @export
list_cons <- function(lst, elem)
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
as.vector.linked_list <- function(x, mode = "any") {
  as.vector(as.list(x), mode)
}

