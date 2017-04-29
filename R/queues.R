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



## Environment queues #################################################

queue_environment <- function(front, back) {
  e <- new.env(parent = emptyenv())
  e$front <- front
  e$back <- back
  class(e) <- c("env_queue", "environment")
  e
}

#' Construct an empty closure based queue
#' @return an empty queue
#' @export
empty_env_queue <- function()
  queue_environment(empty_list(), empty_list())

#' @method is_empty env_queue
#' @export
is_empty.env_queue <- function(x)
  is_empty(x$front) && is_empty(x$back)

#' @method enqueue env_queue
#' @export
enqueue.env_queue <- function(x, elm) {
  x$back <- list_cons(elm, x$back)
  x
}

#' @method front env_queue
#' @export
front.env_queue <- function(x) {
  if (is_empty(x$front)) {
    x$front <- list_reverse(x$back)
    x$back <- empty_list()
  }
  list_head(x$front)
}

#' @method dequeue env_queue
#' @export
dequeue.env_queue <- function(x) {
  if (is_empty(x$front)) {
    x$front <- list_reverse(x$back)
    x$back <- empty_list()
  }
  x$front <- list_tail(x$front)
  x
}



## Closure queues #####################################################

queue <- function(front, back)
  list(front = front, back = back)

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
      q <<- queue(list_tail(list_reverse(q$back)), empty_list())
    } else {
      q <<- queue(list_tail(q$front), q$back)
    }
  }

  structure(list(is_empty = queue_is_empty,
                 get_queue = get_queue,
                 enqueue = enqueue,
                 front = front,
                 dequeue = dequeue),
            class = "closure_queue")
}

#' Construct an empty closure based queue
#' @return an empty queue
#' @export
empty_closure_queue <- function() queue_closure()

#' @method is_empty closure_queue
#' @export
is_empty.closure_queue <- function(x) x$is_empty()

#' @method enqueue closure_queue
#' @export
enqueue.closure_queue <- function(x, elm) {
  x$enqueue(elm)
  x
}

#' @method front closure_queue
#' @export
front.closure_queue <- function(x) x$front()

#' @method dequeue closure_queue
#' @export
dequeue.closure_queue <- function(x) {
  x$dequeue()
  x
}

## Extended (purely functional) queues ################################
queue_extended <- function(x, front, back)
  structure(list(x = x, front = front, back = back),
            class = "extended_queue")


#' Construct an empty extended queue
#'
#' This is just a queue that doesn't use a closure to be able to update
#' the data structure when front is called.
#'
#' @return an empty queue
#' @export
empty_extended_queue <- function() queue_extended(NA, empty_list(), empty_list())

#' @method is_empty extended_queue
#' @export
is_empty.extended_queue <- function(x)
  is_empty(x$front) && is_empty(x$back)

#' @method enqueue extended_queue
#' @export
enqueue.extended_queue <- function(x, elm)
  queue_extended(ifelse(is_empty(x$back), elm, x$x),
                 x$front, list_cons(elm, x$back))

#' @method front extended_queue
#' @export
front.extended_queue <- function(x) {
  if (is_empty(x)) stop("Taking the front of an empty list")
  if (is_empty(x$front)) x$x
  else list_head(x$front)
}

#' @method dequeue extended_queue
#' @export
dequeue.extended_queue <- function(x) {
  if (is_empty(x)) stop("Taking the front of an empty list")
  if (is_empty(x$front))
    x <- queue_extended(NA, list_reverse(x$back), empty_list())
  queue_extended(x$x, list_tail(x$front), x$back)
}

## Lazy queues ###############################################


nil <- function() NULL
cons <- function(car, cdr) {
  force(car)
  force(cdr)
  function() list(car = car, cdr = cdr)
}

is_nil <- function(lst) is.null(lst())
car <- function(lst) lst()$car
cdr <- function(lst) lst()$cdr


cat <- function(l1, l2) {
  force(l1)
  force(l2)
  if (is_nil(l1)) l2
  else {
    lazy_thunk <- function(lst) function() lst()
    lazy_thunk(cons(car(l1), cat(cdr(l1), l2)))
  }
}

lazy_queue <- function(front, back, helper) {
  structure(list(front = front, back = back, helper = helper),
            class = "lazy_queue")
}

rot <- function(front, back, a) {
  if (is_nil(front)) cons(car(back), a)
  else {
    lazy_thunk <- function(lst) function() lst()
    tail <- cons(car(back), a)
    cons(car(front), lazy_thunk(rot(cdr(front), cdr(back), tail)))
  }
}


make_q <- function(front, back, helper) {
  if (is_nil(helper)) {
    helper <- rot(front, back, nil)
    lazy_queue(helper, nil, helper)
  } else {
    lazy_queue(front, back, cdr(helper))
  }
}

#' Creates an empty lazy queue
#' @export
empty_lazy_queue <- function() lazy_queue(nil, nil, nil)

#' @method is_empty lazy_queue
#' @export
is_empty.lazy_queue <- function(x) is_nil(x$front) && is_nil(x$back)

#' @method enqueue lazy_queue
#' @export
enqueue.lazy_queue <- function(x, elm)
  make_q(x$front, cons(elm, x$back), x$helper)

#' @method front lazy_queue
#' @export
front.lazy_queue <- function(x) car(x$front)

#' @method dequeue lazy_queue
#' @export
dequeue.lazy_queue <- function(x)
  make_q(cdr(x$front), x$back, x$helper)
