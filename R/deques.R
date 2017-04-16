# Implementations of deques

#' Add an element to the front of a deque
#' @param x A deque
#' @param elm An element
#' @return an updated deque where the element has been added
#' @export
enqueue_front <- function(x, elm) UseMethod("enqueue_front")

#' Add an element to the front of a deque
#' @param x A deque
#' @param elm An element
#' @return an updated deque where the element has been added
#' @export
enqueue_back <- function(x, elm) UseMethod("enqueue_back")

# #' Get the front element of a deque
# #' @param x A deque
# #' @return the front element of the deque
# #' @export
# front <- function(x) UseMethod("front")

#' Get the back element of a deque
#' @param x A deque
#' @return the back element of the deque
#' @export
back <- function(x) UseMethod("back")

#' Remove the front element of a deque
#' @param x The deque
#' @return The updated deque
#' @export
dequeue_front <- function(x) UseMethod("dequeue_front")

#' Remove the back element of a deque
#' @param x The deque
#' @return The updated deque
#' @export
dequeue_back <- function(x) UseMethod("dequeue_back")


## Environment deques #################################################

deque_environment <- function(front, back, front_length, back_length) {
  e <- new.env(parent = emptyenv())
  e$front <- front
  e$back <- back
  e$front_length <- front_length
  e$back_length <- back_length
  class(e) <- c("env_deque", "environment")
  e
}

#' Construct an empty deque
#' @return an empty deque
#' @export
empty_env_deque <- function()
  deque_environment(empty_list(), empty_list(), 0, 0)

#' @method is_empty env_deque
#' @export
is_empty.env_deque <- function(x)
  is_empty(x$front) && is_empty(x$back)

#' @method enqueue_back env_deque
#' @export
enqueue_back.env_deque <- function(x, elm) {
  x$back <- list_cons(elm, x$back)
  x$back_length <- x$back_length + 1
  x
}

#' @method enqueue_front env_deque
#' @export
enqueue_front.env_deque <- function(x, elm) {
  x$front <- list_cons(elm, x$front)
  x$front_length <- x$front_length + 1
  x
}

move_front_to_back <- function(x) {
  n <- list_length(x$front)
  m <- ceiling(n)
  x$back <- list_get_n_reversed(x$front, m)
  x$front <- list_drop_n(x$front, m)
  x$back_length <- m
  x$front_length <- n - m
}

move_back_to_front <- function(x) {
  n <- list_length(x$back)
  m <- ceiling(n)
  x$front <- list_get_n_reversed(x$back, m)
  x$back <- list_drop_n(x$back, m)
  x$front_length <- m
  x$back_length <- n - m
}

#' @method front env_deque
#' @export
front.env_deque <- function(x) {
  if (is_empty(x$front)) move_back_to_front(x)
  list_head(x$front)
}

#' @method back env_deque
#' @export
back.env_deque <- function(x) {
  if (is_empty(x$back)) move_front_to_back(x)
  list_head(x$back)
}


#' @method dequeue_front env_deque
#' @export
dequeue_front.env_deque <- function(x) {
  if (is_empty(x$front)) move_back_to_front(x)
  x$front <- list_tail(x$front)
  x
}

#' @method dequeue_back env_deque
#' @export
dequeue_back.env_deque <- function(x) {
  if (is_empty(x$back)) move_front_to_back(x)
  x$back <- list_tail(x$back)
  x
}



