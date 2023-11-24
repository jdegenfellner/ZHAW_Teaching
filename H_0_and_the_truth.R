# H_0 and the truth:

library(pacman)
p_load(DiagrammeR, data.tree, tidyverse)

# GPT4:
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


alpha <- 0.05 # Type I decision probability
beta <- 0.2 # 1 - Power
# Probability that the H_0 is true
p <- seq(
  from = 0,
  to = 1,
  length.out = 100
)


# Probability of making the right decision (depending on p) ----
prob_truth_f <- function(p) {
  (p * (1 - alpha) + (1 - p) * (1 - beta)) / (p * (1 - alpha) + (1 - p) * (1 - beta) + p * alpha + (1 - p) * beta)
}
# Note, the denominator simplifies to 1:
prob_truth_f_simpl <- function (p){
  1 + p*(beta - alpha) - beta
}


df <- data.frame(p = p, 
                 prob_truth = prob_truth_f(p), 
                 prob_truth_simpl = prob_truth_f_simpl(p))
ggplot(df, aes(x = p)) +
  geom_line(aes(y = prob_truth), color = "blue", size=1) +  
  geom_point(aes(y = prob_truth_simpl), color = "red", size=0.2) + 
  ggtitle("Probability to make the correct decision") +
  theme(plot.title = element_text(hjust = 0.5))

# Example -----
# if you do not have any clue about p, let us assume p = 0.5
# Probability, to make the wrong decision is:
1 - prob_truth_f(p = 0.5) 
# 0.125 # Many people would think it is 5%

# Irrespective of p, looking at the 4 possible decisions,
# the probability of a incorrect decision would be:
1 - (1 - alpha + 1 - beta)/(1 - alpha + 1- beta + alpha + beta)
# 0.125 # the same as tossing a coin if H_0 is true or not.

