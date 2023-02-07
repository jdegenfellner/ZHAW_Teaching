library(tidyverse)
#library(pillar)

str(chickwts)

tot_mean <- mean(chickwts$weight)

options(digits = 7)

# Slide 11, LM 2
chickwts %>% group_by(feed) %>% 
  summarise(Durchschnitt = mean(weight),
            Effekt = mean(weight) - tot_mean)

boxplot(weight ~ feed, data = chickwts)
