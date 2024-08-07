library(pacman)
p_load(tidyverse, 
       emmeans, # Estimated marginal means (EMMs) for specified factors or factor combinations in a linear model
       flextable, # Nice tables
       gtsummary, # Nice regression results
       performance) # check model assumptions

# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/chickwts.R

# Einfaktorielle Varianzanalyse (ein Faktor: feed)

str(chickwts)
# An experiment was conducted to measure and compare the effectiveness of 
# various feed supplements on the growth rate of chickens.
# Newly hatched chicks were randomly allocated into six groups, and each
# group was given a different feed supplement. Their weights in grams after 
# six weeks are given along with feed types.
# Ref: McNeil, D. R. (1977) Interactive Data Analysis. New York: Wiley.

# Due to the design of the experiment, one could indeed talk about an "effect".
# Very often this characterization is not justified (imho).

tot_mean <- mean(chickwts$weight)
#options(digits = 10) # The digits option controls how many digits are printed (by default); see also: https://www.burns-stat.com/the-options-mechanism-in-r/

# Slide 11, LM2.pdf----
chickwts %>% group_by(feed) %>% 
  summarise(Durchschnitt = mean(weight),
            Effekt = mean(weight) - tot_mean)

# below could be done more elegantly, I guess
chickwts %>%
  group_by(feed) %>%
  summarise(
    Durchschnitt = format(round(mean(weight), digits = 2), nsmall = 2),
    Effekt = format(round(mean(weight) - tot_mean, digits = 2), nsmall = 2)
  )


# Slide 12----
boxplot(weight ~ feed, data = chickwts)

# same, a little more modern
chickwts %>% ggplot(aes(x = feed, y = weight)) +
  geom_boxplot() + 
  geom_jitter(size = 0.4) + # Show points
  coord_flip() + # Rotate 90 degrees, looks nice
  theme_dark() +  # Change theme for fun, looks even nicer
  theme(plot.title = element_text(hjust = 0.5)) + # Center title
  geom_hline(yintercept = tot_mean, color = "red") # Add red line indicating the total mean

# Slide 13-----
# Reference level of feed?
levels(chickwts$feed) # Reference is the first level of the factor -> levels(x)[1]

# How can you change the reference level?
chickwts$feed_ <- relevel(chickwts$feed, ref="soybean")

modOne <- lm(weight ~ feed, data = chickwts) # "full" model
mean(chickwts$weight) # 261.3098592 != intercept
summary(modOne) # Effects relative to reference level (casein) mean!

# Intercept manually:
chickwts %>% 
  filter(feed == "casein") %>%
  summarise(mean_casein = mean(weight))

tbl_regression(modOne) # Shows reference level "casein" nicely

predict(modOne, newdata = data.frame(feed = "sunflower")) # same as in Slide 11, LM2.pdf above
# Predict manually for "sunflower?
# "casein" is the reference level, hence:
323.5833 + 5.333 # same result!
predict(modOne, newdata = data.frame(feed = "soybean"))
predict(modOne, newdata = data.frame(feed = "meatmeal"))
predict(modOne, newdata = data.frame(feed = "linseed"))                    
predict(modOne, newdata = data.frame(feed = "horsebean"))
predict(modOne, newdata = data.frame(feed = "casein"))

# Means- vs. Effect parametrization----

# standard in R: reference coding (ref level for feed: casein)
modOne <- lm(weight ~ feed, data = chickwts) # "full" model
summary(modOne)

# means parametrization(?):
modMean <- lm(weight ~ -1 + feed, data = chickwts)
summary(modMean) # looks like just the means:
chickwts <- as.data.table(chickwts)
mean(chickwts[feed == "casein",]$weight)
mean(chickwts[feed == "horsebean",]$weight)
# ...
# contrasts(chickwts$feed) <- contr.treatment(levels(chickwts$feed)) # set back to default.



# Slide 15----
mod0 <- update(modOne, . ~ . - feed) # only intercept model, just do a mean of weight
anova(mod0, modOne)
drop1(modOne, test = "F") # should the results include a test statistic relative to the original model?

# Same, but possibly easier syntax:
mod0 <- lm(weight ~ 1, data = chickwts) # 1.. just take the mean as predictor
anova(mod0, modOne)

# What are the H0 and H1 here?



# Slide 18-----
em <- emmeans(modOne, pairwise ~ feed) 
summary(em, infer = c(TRUE, TRUE))$contrasts
# Large differences from the boxplot above get rather low p-values
# as expected.

# Slide 19----
plot(em$contrasts)

# Slide Residuenanalyse ----
check_normality(residuals(modOne)) # OK: raw appear as normally distributed (p = 0.627).
plot(residuals(modOne)) # looks like noice without structure
qqnorm(residuals(modOne))
qqline(residuals(modOne)) # looks fine
check_model(modOne) # interestingly, the "Normality of Residuals" plot seems to indicate some structure, at least visually.

car::leveneTest(weight ~ feed, chickwts)
