tree <- empty_search_tree()
for (i in 1:10)
  tree <- insert(tree, i)
plot(tree)

tree <- empty_red_black_tree()
for (i in 1:10)
  tree <- insert(tree, i)
plot(tree)

tree <- empty_leftist_heap()
for (i in 1:10)
  tree <- insert(tree, i)
plot(tree)

tree <- empty_splay_heap()
for (i in 1:10)
  tree <- insert(tree, i)
plot(tree)
