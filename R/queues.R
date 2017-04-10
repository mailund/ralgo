# Implementations of queues

#' Add an element to a queue
#' @param x A queue
#' @param elm An element
#' @return an updated queue where the element has been added
#' @export
enqueue <- function(x, elm) UseMethod("enqueue")

#' Get the front element of a queue
#' @param x A queue
#' @return the front element of the queue
#' @export
front <- function(x) UseMethod("front")

#' Remove the front element of a queue
#' @param x The queue
#' @return The updated queue
#' @export
dequeue <- function(x) UseMethod("dequeu")

queue <- function(front, back)
  structure(list(front = front, back = back),
            class = "queue")

#' Construct an empty queue
#' @return an empty queue
#' @export
empty_queue <- function()
  queue(empty_list(), empty_list())

#' @method is_empty queue
#' @export
is_empty.queue <- function(x) is_empty(x$front) && is_empty(x$back)

#' @method enqueue queue
#' @export
enqueue.queue <- function(x, elm) {
  queue(x$front, list_cons(elm, x$back))
}

#' @method front queue
#' @export
front <- function(x) {
  stop("I am not sure how to implement this yet")
}

#' @method dequeue queue
#' @export
dequeue <- function(x) {
  stop("I am not sure how to implement this yet")
}
