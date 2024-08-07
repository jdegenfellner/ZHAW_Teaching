# Some code for simple linear regression

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Current version see:
# https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Simple_Linear_Regression.R
# 24.1.24
# QM2 
# See also Script QM2 (Meichtry 2022, pages.2-13)

# Example data for simple linear regression----
# https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regressionSimple.csv
# https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regression.csv
# or just make up your own data...

library(pacman)
p_load(plotly, tidyverse)

# Page 5 Script ----
# Let's look at formula (1.2.6) and use data from above
df <- read.csv("https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regressionSimple.csv")
str(df)
df
plot(df$x,df$y)
mod <- lm(y ~ x, data = df) # least squares method.
summary(mod)
sum((mod$fitted.values - df$y)^2) # true minimum of SSE (= sum of squared errors)
# 5.837053 (= z_min in Plot below)

sse <- function(alpha, beta, data) {
  predicted <- alpha + beta * data$x
  sum((data$y - predicted)^2)
}

alpha_values <- seq(-2, 2, length.out = 1000)
beta_values <- seq(-2, 2, length.out = 1000)
sse_values <- outer(alpha_values, beta_values, Vectorize(function(a, b) sse(a, b, df)))
fig <- plot_ly(x = ~alpha_values, y = ~beta_values, z = ~sse_values, type = "surface") %>%
       layout(title = "Sum of squared errors depending on alpha and beta")
fig

# Highlight min of this paraboloid (fig)----
min_sse_index <- which(sse_values == min(sse_values), arr.ind = TRUE) # Find indices where error is smallest.
(min_alpha <- alpha_values[min_sse_index[1]])
(min_beta <- beta_values[min_sse_index[2]])
(min_sse <- min(sse_values))

min_alpha # (= Intercept)
min_beta
coef(mod) # match

fig <- fig %>% add_trace(
  x = min_beta, 
  y = min_alpha, 
  z = min_sse, 
  type = 'scatter3d', 
  mode = 'markers',
  marker = list(color = 'red', size = 7)
)

fig

# page 6----
A <- c(0, 0.2, 0.5, 0.7, 1, 1.4, 1.8, 2.25, 2.5)
R <- c(554, 581, 589, 628, 623, 687, 692, 734, 812)
ARdata <- data.frame(Alkohol = A, Reaktionszeit = R)
#plot(A,R)
modAR <- lm(R ~ A, data = ARdata) # Regress R on A
modAR
summary(modAR)

# page 7----
cov(R,A)/var(A) # = beta_hat (1.2.6 Script)
mean(R) - cov(R,A)/var(A) * mean(A) # = alpha_hat (1.2.7 Script)

str(modAR)
fitted(modAR) # \hat{y}_i
residuals(modAR) # y_i - \hat{y}_i
residuals(modAR)
R - fitted(modAR) # residuals

ARdata$predicted <- predict(modAR, ARdata)
ARdata$residuals <- ARdata$R - ARdata$predicted

# page 8----
ggplot(ARdata, aes(x = A, y = R)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = A, yend = predicted), color = "red") + # Add vertical lines
  labs(x = "A [Promille]", y = "R [ms]", 
       title = "Alkoholkonzentration vs. Reaktionszeit") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


new <- c(0.3, 0.9, 1.6) # new A-values
pred.frame <- data.frame(A = new)
pred <- predict(modAR, newdata = pred.frame)
prediction_data <- data.frame(new, pred)

# page 9----
ggplot(ARdata, aes(x = A, y = R)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_segment(aes(xend = A, yend = predicted), color = "red") + # Add vertical lines
  labs(x = "A [Promille]", y = "R [ms]", 
       title = "Alkoholkonzentration vs. Reaktionszeit") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add red triangles for the predicted values
  geom_point(data = prediction_data, aes(x = new, y = pred), 
             shape = 17, color = "red", size = 4)

predict(modAR, newdata = pred.frame, interval = "prediction")

# -> shiny app.
# https://jdegenfellner.shinyapps.io/shiny-app-code_lm/
# Meaning/influence of parameters?
# Refresh (often) to see what happens to Pr(>|t|)!


# fuer die Interessierten:
# (1.2.8) im Skript empirisch prüfen
str(modAR)
sum(modAR$fitted.values*modAR$residuals) # ~ 0 # https://de.wikipedia.org/wiki/Lineare_Einfachregression#Statistische_Eigenschaften_der_Kleinste-Quadrate-Sch%C3%A4tzer
sum(modAR$residuals) # ~ 0

