
#' Add an element to the top of a stack.
#' @param x The stack
#' @param elm The elememtn
#' @return an updated stack
#' @export
push <- function(x, elm) UseMethod("push")

#' Remove the top element from a stack.
#' @param x The stack
#' @return The updated stack
#' @export
pop <- function(x) UseMethod("pop")

#' Get the top element of a stack.
#' @param x The stack
#' @return The top element of the stack
#' @export
top <- function(x) UseMethod("top")

stack_cons <- function(elem, lst)
  structure(list(head = elem, tail = lst),
            class = c("stack", "linked_list"))

stack_nil <- stack_cons(NA, NULL)

#' @method is_empty stack
#' @export
is_empty.stack <- function(x) identical(x, stack_nil)

#' Create an empty stack.
#' @return an empty stack.
#' @export
empty_stack <- function() stack_nil

#' @method push stack
#' @export
push.stack <- function(x, elm) stack_cons(elm, x)

#' @method pop stack
#' @export
pop.stack <- function(x) list_tail(x)

#' @method top stack
#' @export
top.stack <- function(x) list_head(x)


