# H_0 and the truth:

library(pacman)
p_load(DiagrammeR, data.tree, tidyverse)

alpha <- 0.05
beta <- 0.2
p <- seq(0,1,length.out=100)

# Create the tree structure
tree <- Node$new("H_0 True?")
tree$AddChild("P")
tree$AddChild("1-P")
tree$children[[1]]$AddChild("1-alpha")
tree$children[[1]]$AddChild("alpha")
tree$children[[2]]$AddChild("1-beta")
tree$children[[2]]$AddChild("beta")
graph <- ToDiagrammeRGraph(tree)
render_graph(graph)

# probability of making the right decision (depending on p)

prob_truth <- (p*(1-alpha)+(1-p)*(1-beta))/(p*(1-alpha)+(1-p)*(1-beta) + p*alpha + (1-p)*beta)

df <- data.frame(p = p, prob_truth = prob_truth)
df %>% ggplot(aes(x=p, y=prob_truth)) + 
  geom_point()
