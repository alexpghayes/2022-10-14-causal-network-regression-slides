# library(fastRG)

library(ggplot2)
library(igraph)

set.seed(27)

B <- matrix(0.02, 2, 2)
diag(B) <- 0.6

n <- 4
k <- 2

m <- sbm(n = n, B = B, k = k, poisson_edges = FALSE)
m$z

EA <- expectation(m)
EA

plot_expectation(m) +
  scale_fill_gradient2(high = "black", limits = c(0, 1)) +
  theme_void() +
  theme(legend.position = "none")

set.seed(34)

g <- sample_igraph(m)
g

A <- igraph::as_adj(g)

plot_sparse_matrix(A) +
  theme_void() +
  theme(legend.position = "none")

# from https://kateto.net/netscix2016.html

V(g)$size <- 8
V(g)$frame.color <- "white"
V(g)$color <- "orange"
V(g)$label <- ""
E(g)$arrow.mode <- 0

plot(g)
