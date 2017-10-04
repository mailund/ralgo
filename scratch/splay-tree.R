tree <- empty_splay_tree()
for (x in 1:10)
  tree <- insert(tree, x)

(p1 <- plot(tree) + ylim(-0.5, 10.5))

member(tree, 4)
(p2 <- plot(tree)+ ylim(-0.5, 5.5))

member(tree, 8)
(p3 <- plot(tree) + ylim(-0.5, 5.5))

member(tree, 6)
(p4 <- plot(tree) + ylim(-0.5, 5.5))

library(ggpubr)
ggarrange(p1, p2, p3, p4, labels = LETTERS[1:4])
