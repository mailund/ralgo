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
dequeue <- function(x) UseMethod("dequeue")

queue <- function(front, back)
  structure(list(front = front, back = back),
            class = "queue")

queue_closure <- function() {
  q <- queue(empty_list(), empty_list())

  get_queue <- function() q

  queue_is_empty <- function() is_empty(q$front) && is_empty(q$back)

  enqueue <- function(elm) {
    q <<- queue(q$front, list_cons(elm, q$back))
  }

  front <- function() {
    if (queue_is_empty()) stop("Taking the front of an empty list")
    if (is_empty(q$front)) {
      q <<- queue(list_reverse(q$back), empty_list())
    }
    list_head(q$front)
  }

  dequeue <- function() {
    if (queue_is_empty()) stop("Taking the front of an empty list")
    if (is_empty(q$front)) {
      q <<- queue(list_reverse(q$back), empty_list())
    }
    q$front <<- list_tail(q$front)
  }

  structure(list(is_empty = queue_is_empty,
                 get_queue = get_queue,
                 enqueue = enqueue,
                 front = front,
                 dequeue = dequeue),
            class = "queue")
}

#' Construct an empty queue
#' @return an empty queue
#' @export
empty_queue <- function() queue_closure()

#' @method is_empty queue
#' @export
is_empty.queue <- function(x) x$queue_is_empty()

#' @method enqueue queue
#' @export
enqueue.queue <- function(x, elm) {
  x$enqueue(elm)
  x
}

#' @method front queue
#' @export
front.queue <- function(x) x$front()

#' @method dequeue queue
#' @export
dequeue.queue <- function(x) {
  x$dequeue()
  x
}
