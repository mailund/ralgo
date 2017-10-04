
# States
IDLE = 0
REVERSING = 1
APPENDING = 2
DONE = 3

idle_state <- function() list(state = IDLE)
reversing_state <- function(
  keep
  , front
  , reverse_front
  , back
  , reverse_back) {
  list(state = REVERSING,
       keep = keep,
       front = front,
       reverse_front = reverse_front,
       back = back,
       reverse_back = reverse_back)
}
appending_state <- function(
  keep
  , front
  , back) {
  list(state = APPENDING,
       keep = keep,
       front = front,
       back = back)
}
done_state <- function(result) {
  list(state = DONE, result = result)
}

rebuild_queue_node <- function(
  state
  , front_size
  , front
  , back_size
  , back) {
  structure(list(state = state,
                 front_size = front_size,
                 front = front,
                 back_size = back_size,
                 back = back),
            class = "rebuild_queue")
}

#' Create an empty "global rebuild" queue
#' @return empty quuee
#' @export
empty_rebuild_queue <- function() {
  rebuild_queue_node(state = idle_state(),
                     front_size = 0,
                     front = empty_list(),
                     back_size = 0,
                     back = empty_list())
}

#' @method is_empty rebuild_queue
#' @export
is_empty.rebuild_queue <- function(x) is_empty(x$front)

exec <- function(state) {
  if (state$state == REVERSING) {
    if (is_empty(state$front)) {
      # we are done and we just need to move one missing element from back
      stopifnot(is_empty(list_tail(state$back)))
      appending_state(keep = state$keep,
                      front = state$reverse_front,
                      back = list_cons(list_head(state$back), state$reverse_back))
    } else {
      reversing_state(keep = state$keep + 1,
                      front = list_tail(state$front),
                      reverse_front = list_cons(list_head(state$front), state$reverse_front),
                      back = list_tail(state$back),
                      reverse_back = list_cons(list_head(state$back), state$reverse_back))
    }
  } else if (state$state == APPENDING) {
    if (state$keep == 0) {
      done_state(result = state$back)
    } else {
      appending_state(keep = state$keep - 1,
                      front = list_tail(state$front),
                      back = list_cons(list_head(state$front), state$back))
    }
  } else {
    state
  }
}

invalidate <- function(state) {
  if (state$state == REVERSING) {
    reversing_state(keep = state$keep - 1,
                    front = state$front,
                    reverse_front = state$reverse_front,
                    back = state$back,
                    reverse_back = state$reverse_back)
  } else if (state$state == APPENDING) {
    if (state$keep == 0) {
      done_state(result = list_tail(state$back))
    } else {
      appending_state(keep = state$keep - 1,
                      front = state$front,
                      back = state$back)
    }
  } else {
      state
  }
}

exec2 <- function(x) {
  new_state <- exec(exec(x$state))
  if (new_state$state == DONE)
    rebuild_queue_node(state = idle_state(),
                       front_size = x$front_size,
                       front = new_state$result,
                       back_size = x$back_size,
                       back = x$back)
  else
    rebuild_queue_node(state = new_state,
                       front_size = x$front_size,
                       front = x$front,
                       back_size = x$back_size,
                       back = x$back)
}

check <- function(x) {
  if (x$back_size <= x$front_size) {
    exec2(x)
  } else {
    # when back gets longer than front, we start reversing
    new_state <- reversing_state(keep = 0,
                                 front = x$front,
                                 reverse_front = empty_list(),
                                 back = x$back,
                                 reverse_back = empty_list())
    new_queue <- rebuild_queue_node(state = new_state,
                                    front_size = x$front_size + x$back_size,
                                    front = x$front,
                                    back_size = 0,
                                    back = empty_list())
    exec2(new_queue)
  }
}

#' @method enqueue rebuild_queue
#' @export
enqueue.rebuild_queue <- function(x, elm) {
  check(rebuild_queue_node(state = x$state,
                           front_size = x$front_size,
                           front = x$front,
                           back_size = x$back_size + 1,
                           back = list_cons(elm, x$back)))
}

#' @method front rebuild_queue
#' @export
front.rebuild_queue <- function(x) {
  list_head(x$front)
}

#' @method dequeue rebuild_queue
#' @export
dequeue.rebuild_queue <- function(x) {
  new_queue <- rebuild_queue_node(state = invalidate(x$state),
                                  front_size = x$front_size - 1,
                                  front = list_tail(x$front),
                                  back_size = x$back_size,
                                  back = x$back)
  check(new_queue)
}

