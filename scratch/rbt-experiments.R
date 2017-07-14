
tree <- empty_red_black_tree()
for (i in 1:4)
  tree <- insert(tree, i)
plot(tree)

plot(remove(tree, 0))
plot(remove(tree, 1))
plot(remove(tree, 2))
plot(remove(tree, 3))
plot(remove(tree, 4))
plot(remove(tree, 5))
plot(remove(tree, 6))
