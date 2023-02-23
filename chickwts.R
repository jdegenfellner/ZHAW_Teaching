library(tidyverse)
library(flextable)
library(gtsummary)


str(chickwts)
tot_mean <- mean(chickwts$weight)
options(digits = 7)

# Slide 11, LM2.pdf
chickwts %>% group_by(feed) %>% 
  summarise(Durchschnitt = mean(weight),
            Effekt = mean(weight) - tot_mean)
# Slide 12
boxplot(weight ~ feed, data = chickwts)


# Slide 12, LM2.pdf
chickwts %>% ggplot(aes(x = feed, y = weight)) +
  geom_boxplot() + 
  geom_jitter(size = 0.4) + # Show points
  ggtitle("Boxplot with ggplot") + 
  coord_flip() + # Rotate 90 degrees, looks nice
  theme_dark() +  # Change theme for fun, looks even nicer
  theme(plot.title = element_text(hjust = 0.5)) + # Center title
  geom_hline(yintercept = tot_mean, color = "red") # Add red line indicating the total mean

# Side 13, LM2.pdf
mod <- lm(weight ~ feed, data = chickwts)
summary(mod)
tbl_regression(mod) # Shows reference level "casein" nicely
predict(mod, newdata = data.frame(feed = "sunflower")) # same as in Slide 11, LM2.pdf above
# predict manually for "sunflower?
# "casein" is the reference level, hence:
323.5833 + 5.333 # same result!
predict(mod, newdata = data.frame(feed = "soybean"))
predict(mod, newdata = data.frame(feed = "meatmeal"))
predict(mod, newdata = data.frame(feed = "linseed"))                    
predict(mod, newdata = data.frame(feed = "horsebean"))
predict(mod, newdata = data.frame(feed = "casein"))

# Slide 18, LM2.pdf
library(emmeans)
em <- emmeans(mod, pairwise ~ feed) 
summary(em, infer = c(TRUE, TRUE))$contrasts
# Large differences from the boxplot above get rather low p-values
# as expected.
