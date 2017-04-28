# This file contains some basic data structures that the
# abstract data structures elsewhere are based upon, plus some generic functions
# used by all data structures

## Generic functions for general data structures ###########################

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

#' Check membership in a data structure
#' @param x The data structure
#' @param elm Element to check membership for
#' @param ... Potential optional arguments
#' @return the merged x and y
#' @export
member <- function(x, elm, ...) UseMethod("member")

#' Remove an element from a data structure
#' @param x The data structure
#' @param elm Element to remove
#' @param ... Potential optional arguments
#' @return x with elm removed
#' @export
remove <- function(x, elm, ...) UseMethod("remove")

#' Compute the length of a linked list
#' @param lst The list
#' @return the length of the list
#' @export
list_length <- function(lst) UseMethod("list_length")



## Linked lists #########################

#' Add a head item to a linked list.
#' @param elem  The item to put at the head of the list.
#' @param lst   The list -- it will become the tail of the new list.
#' @return a new linked list.
#' @export
list_cons <- function(elem, lst)
  structure(list(head = elem, tail = lst), class = "linked_list")

list_nil <- list_cons(NA, NULL)

#' @method is_empty linked_list
#' @export
is_empty.linked_list <- function(x) identical(x, list_nil)

#' Create an empty linked list.
#' @return an empty linked list.
#' @export
empty_list <- function() list_nil


#' Get the item at the head of a linked list.
#' @param lst The list
#' @return The element at the head of the list.
#' @export
list_head <- function(lst) lst$head

#' Get the tail of a linked list.
#' @param lst The list
#' @return The tail of the list
#' @export
list_tail <- function(lst) lst$tail

#' @method list_length linked_list
#' @export
list_length.linked_list <- function(lst) {
  n <- 0
  while (!is_empty(lst)) {
    lst <- lst$tail
    n <- n + 1
  }
  n
}

#' Get a new list containing the first n elements of a list.
#' @param lst A list
#' @param n the number of elements to get
#' @return a new list containing the first n elements of lst.
#' @export
list_get_n <- function(lst, n) {
  l <- empty_list()
  while (n > 0) {
    l <- list_cons(list_head(lst), l)
    lst <- list_tail(lst)
    n <- n - 1
  }
  list_reverse(l)
}


#' Get a new list containing the first n elements of a list reversed.
#' @param lst A list
#' @param n the number of elements to get
#' @return a new list containing the first n elements of lst.
#' @export
list_get_n_reversed <- function(lst, n) {
  l <- empty_list()
  while (n > 0) {
    l <- list_cons(list_head(lst), l)
    lst <- list_tail(lst)
    n <- n - 1
  }
  l
}

#' Get a new list containing the elements when n elements have been removed from a list/
#' @param lst A list
#' @param n the number of elements to remove
#' @return a new list containing the elements of lst except the first n
#' @export
list_drop_n <- function(lst, n) {
  l <- lst
  while (n > 0) {
    l <- list_tail(l)
    n <- n - 1
  }
  l
}

#' Construct a list that is the concatenation of two other lists.
#'
#' This function is linear in the length of the first list.
#'
#' @param l1 The first list.
#' @param l2 The second list.
#' @return The concatenation of l1 and l2.
#' @export
list_concatenate <- function(l1, l2) {
    rev_l1 <- empty_list()
    while (!is_empty(l1)) {
      rev_l1 <- list_cons(list_head(l1), rev_l1)
      l1 <- list_tail(l1)
    }
    result <- l2
    while (!is_empty(rev_l1)) {
      result <- list_cons(list_head(rev_l1), result)
      rev_l1 <- list_tail(rev_l1)
    }
    result
}

#' @method remove linked_list
#' @export
remove.linked_list <- function(x, elm, ...) {
  if (is_empty(x)) x
  else if (list_head(x) == elm) list_tail(x)
  else list_cons(list_head(x), remove(list_tail(x), elm))
}

#' Reverse a list
#' @param lst A list
#' @return the reverse of lst
#' @export
list_reverse <- function(lst) {
  acc <- empty_list()
  while (!is_empty(lst)) {
    acc <- list_cons(list_head(lst), acc)
    lst <- list_tail(lst)
  }
  acc
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
  while (!is_empty(x)) {
    result[i] <- x$head
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


## Helper functions ##############################
pattern_match <- function(...) {
  bindings <- eval(substitute(alist(...)))
  scope <- parent.frame()

  var_names <- names(bindings)
  for (i in seq_along(bindings)) {
    name <- var_names[i]
    val <- eval(bindings[[i]], scope)

    if (is.null(val)) return(FALSE)

    # for expressions that are not assignments, we consider them conditions
    # that must be true for the pattern to match. Return FALSE if they are not.
    if (nchar(name) == 0 && !val) return(FALSE)
    else if (nchar(name) > 0) assign(name, val, envir = scope)

  }
  return(TRUE)
}



## Lists with the length remembered #########################
## Linked lists #########################

#' Add a head item to a linked list.
#' @param elem  The item to put at the head of the list.
#' @param lst   The list -- it will become the tail of the new list.
#' @return a new linked list.
#' @export
list_cons <- function(elem, lst)
  structure(list(head = elem, tail = lst), class = "linked_list")

list_nil <- list_cons(NA, NULL)

#' @method is_empty linked_list
#' @export
is_empty.linked_list <- function(x) identical(x, list_nil)

#' Create an empty linked list.
#' @return an empty linked list.
#' @export
empty_list <- function() list_nil


#' Get the item at the head of a linked list.
#' @param lst The list
#' @return The element at the head of the list.
#' @export
list_head <- function(lst) lst$head

#' Get the tail of a linked list.
#' @param lst The list
#' @return The tail of the list
#' @export
list_tail <- function(lst) lst$tail

#' @method list_length linked_list
#' @export
list_length.linked_list <- function(lst) {
  n <- 0
  while (!is_empty(lst)) {
    lst <- lst$tail
    n <- n + 1
  }
  n
}

#' Get a new list containing the first n elements of a list.
#' @param lst A list
#' @param n the number of elements to get
#' @return a new list containing the first n elements of lst.
#' @export
list_get_n <- function(lst, n) {
  l <- empty_list()
  while (n > 0) {
    l <- list_cons(list_head(lst), l)
    lst <- list_tail(lst)
    n <- n - 1
  }
  list_reverse(l)
}


#' Get a new list containing the first n elements of a list reversed.
#' @param lst A list
#' @param n the number of elements to get
#' @return a new list containing the first n elements of lst.
#' @export
list_get_n_reversed <- function(lst, n) {
  l <- empty_list()
  while (n > 0) {
    l <- list_cons(list_head(lst), l)
    lst <- list_tail(lst)
    n <- n - 1
  }
  l
}

#' Get a new list containing the elements when n elements have been removed from a list/
#' @param lst A list
#' @param n the number of elements to remove
#' @return a new list containing the elements of lst except the first n
#' @export
list_drop_n <- function(lst, n) {
  l <- lst
  while (n > 0) {
    l <- list_tail(l)
    n <- n - 1
  }
  l
}

#' Construct a list that is the concatenation of two other lists.
#'
#' This function is linear in the length of the first list.
#'
#' @param l1 The first list.
#' @param l2 The second list.
#' @return The concatenation of l1 and l2.
#' @export
list_concatenate <- function(l1, l2) {
  rev_l1 <- empty_list()
  while (!is_empty(l1)) {
    rev_l1 <- list_cons(list_head(l1), rev_l1)
    l1 <- list_tail(l1)
  }
  result <- l2
  while (!is_empty(rev_l1)) {
    result <- list_cons(list_head(rev_l1), result)
    rev_l1 <- list_tail(rev_l1)
  }
  result
}

#' @method remove linked_list
#' @export
remove.linked_list <- function(x, elm, ...) {
  if (is_empty(x)) x
  else if (list_head(x) == elm) list_tail(x)
  else list_cons(list_head(x), remove(list_tail(x), elm))
}

#' Reverse a list
#' @param lst A list
#' @return the reverse of lst
#' @export
list_reverse <- function(lst) {
  acc <- empty_list()
  while (!is_empty(lst)) {
    acc <- list_cons(list_head(lst), acc)
    lst <- list_tail(lst)
  }
  acc
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
  while (!is_empty(x)) {
    result[i] <- x$head
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


## Helper functions ##############################
pattern_match <- function(...) {
  bindings <- eval(substitute(alist(...)))
  scope <- parent.frame()

  var_names <- names(bindings)
  for (i in seq_along(bindings)) {
    name <- var_names[i]
    val <- eval(bindings[[i]], scope)

    if (is.null(val)) return(FALSE)

    # for expressions that are not assignments, we consider them conditions
    # that must be true for the pattern to match. Return FALSE if they are not.
    if (nchar(name) == 0 && !val) return(FALSE)
    else if (nchar(name) > 0) assign(name, val, envir = scope)

  }
  return(TRUE)
}



