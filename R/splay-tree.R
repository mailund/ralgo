
#' Create an empty splay search tree.
#' @return an empty splay tree
#' @export
empty_splay_tree <- function() {
  ref <- new.env(parent = emptyenv())
  ref$tree <- empty_splay_node()
  structure(ref, class = c("splay_tree", "search_tree"))
}

#' @method is_empty splay_tree
#' @export
is_empty.splay_tree <- function(x)
  is_empty(x$tree)

#' @method insert splay_tree
#' @export
insert.splay_tree <- function(x, elm, ...) {
  if (member(x, elm))
    return(x) # don't insert if we already have the element

  part <- partition(elm, x$tree)
  x$tree <- splay_tree_node(
    value = elm,
    left = part$smaller,
    right = part$larger
  )
  x
}


splay_remove <- function(tree, elm) {
  # if we reach an empty tree, there is nothing to do
  if (is_empty(tree)) return(tree)

  if (tree$value == elm) {
    a <- tree$left
    b <- tree$right
    if (is_empty(a)) return(b)
    if (is_empty(b)) return(a)

    s <- st_leftmost(tree$right)
    return(splay_tree_node(s, a, splay_remove(b, s)))
  }

  # we need to search further down to remove the element
  if (elm < tree$value)
    splay_tree_node(tree$value, splay_remove(tree$left, elm), tree$right)
  else # (elm > tree$value)
    splay_tree_node(tree$value, tree$left, splay_remove(tree$right, elm))
}


#' @method remove splay_tree
#' @export
remove.splay_tree <- function(x, elm, ...) {
  x$tree <- splay_remove(x$tree, elm)
  x
}

splay <- function(tree, v) {
  if (is_empty(tree) || tree$value == v) {
    tree # if v is already the root, we are done splaying
         # we are also done if we reach an empty tree;
         # then v is not in the tree

  # -- Zig-zig -------------------------------------
  } else if (pattern_match(z = tree$value,
                           y = tree$left$value,
                           z > v && y > v,

                           s = splay(tree$left$left, v),
                           x = s$value,

                           a = s$left,
                           b = s$right,
                           c = tree$left$right,
                           d = tree$right)) {
    splay_tree_node(
      value = x,
      left = a,
      right = splay_tree_node(
        value = y,
        left = b,
        right = splay_tree_node(
          value = z,
          left = c,
          right = d)))

  # -- Zag-zag -------------------------------------
  } else if (pattern_match(z = tree$value,
                           y = tree$right$value,
                           z < v && y < v,

                           s = splay(tree$right$right, v),
                           x = s$value,

                           a = s$left,
                           b = s$right,
                           c = tree$right$left,
                           d = tree$left)) {
    splay_tree_node(
      value = x,
      left = splay_tree_node(
        value = y,
        left = splay_tree_node(
          value = z,
          left = d,
          right = c),
        right = a),
      right = b)

  # -- Zig-zag & zag-zig ------------------------
  } else if (pattern_match(z = tree$value,
                           y = tree$left$value,
                           v < z && v > y,

                           s = splay(tree$left$right, v),
                           x = s$value,

                           a = tree$left$left,
                           b = s$left,
                           c = s$right,
                           d = tree$right)
             ||
             pattern_match(y = tree$value,
                           z = tree$right$value,
                           y < v && z > v,

                           s = splay(tree$right$left, v),
                           x = s$value,

                           a = tree$left,
                           b = s$left,
                           c = s$right,
                           d = tree$right$right)) {
  splay_tree_node(
      value = x,
      left = splay_tree_node(
        value = y,
        left = a,
        right = b),
      right = splay_tree_node(
        value = z,
        left = c,
        right = d))

  # -- Zig --------------------------------------
  } else if (pattern_match(y = tree$value,
                           y > v,

                           s = splay(tree$left, v),
                           x = s$value,

                           a = s$left,
                           b = s$right,
                           c = tree$right)) {
    splay_tree_node(
      value = x,
      left = a,
      right = splay_tree_node(
        value = y,
        left = b,
        right = c))

  # -- Zag --------------------------------------
  } else if (pattern_match(y = tree$value,
                           y < v,

                           s = splay(tree$right, v),
                           x = s$value,

                           a = tree$left,
                           b = s$left,
                           c = s$right)) {
    splay_tree_node(
      value = x,
      left = splay_tree_node(
        value = y,
        left = a,
        right = b),
      right = c)

  } else {
    # if the recursive splay operation returns an empty tree,
    # which can happen if v is not in the tree, we reach this point
    # and here we just give up and return the tree.
    tree

  }
}

#' @method member splay_tree
#' @export
member.splay_tree <- function(x, elm, ...) {
  x$tree <- splay(x$tree, elm)
  # if elm is in the tree it is now at the root
  !is.na(x$tree$value) && x$tree$value == elm
}

#' @method plot splay_tree
#' @export
plot.splay_tree <- function(x, ...) {
  plot(x$tree) # nocov
}
