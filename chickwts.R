library(tidyverse)
#library(pillar)

str(chickwts)

tot_mean <- mean(chickwts$weight)

options(digits = 7)

# Slide 11, LM2.pdf
chickwts %>% group_by(feed) %>% 
  summarise(Durchschnitt = mean(weight),
            Effekt = mean(weight) - tot_mean)

boxplot(weight ~ feed, data = chickwts)

# Slide 12, LM2.pdf
chickwts %>% ggplot(aes(x = feed, y = weight)) +
  geom_boxplot() + 
  geom_jitter(size = 0.4) + # show points
  ggtitle("Boxplot with ggplot") + 
  theme(plot.title = element_text(hjust = 0.5)) + # center title
  coord_flip() + # rotate 90 degrees, looks nicer
  theme_dark() # change theme for fun, looks even nicer
  
                    