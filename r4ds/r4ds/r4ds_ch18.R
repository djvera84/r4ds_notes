# R for Data Science Chapter 18
# Model Basics with modelr
# Daniel J. Vera, Ph.D.
library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(splines)

# A Simple Model ----------------------------------------------------------

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4
  ) +
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

# overlay the 10 best models on the data.
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
  ) %>% 
  mutate(dist = purrr::map2_dbl( a1, a2, sim1_dist))

grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, color = "red"
  ) +
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# Exercises 23.2.1 on website:
# http://r4ds.had.co.nz/model-basics.html#exercises-60
# 1. One downside of the linear model is that it is sensitive to unusual
# values because the distance incorporates a squared term. Fit a linear 
# model to the simulated data below, and visualise the results. Rerun a
# few times to generate different simulated datasets. What do you notice
# about the model?

sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
  )

# Answer:
sim1a
sim1a_lm <- lm(y ~ x, data = sim1a)
coef(sim1a_lm)

ggplot(sim1a, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

simt <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2),
    .id = i
  )
}

lm_df <- function(.data) {
  mod <- lm(y ~ x, data = .data)
  beta <- coef(mod)
  tibble(intercept = beta[1], slope = beta[2])
}

sims <- map(1:100, simt) %>%
  map_df(lm_df)

ggplot(sims, aes(x = intercept, y = slope)) +
  geom_point()

# 2. One way to make linear models more robust is to use a different 
# distance measure. For example, instead of root-mean-squared distance,
# you could use mean-absolute distance:
measure_distance <- function(mod, data) {
  diff <- data$y - make_prediction(mod, data)
  mean(abs(diff))
}

# Use optim() to fit this model to the simulated data above and compare it
# to the linear model.

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

measure_distance_mad <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}
measure_distance_mad(c(7, 1.5), sim1)

sim1_dist_mad <- function(a1, a2) {
  measure_distance_mad(c(a1, a2), sim1)
}

models_mad <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist_mad))
models_mad

models_se <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models_se

# overlay the 10 best models on the data.
ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = filter(models_se, rank(dist) <= 5),
    color = "blue"
  ) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = filter(models_mad, rank(dist) <= 5),
    color = "red"
  )

models_compare <- models_se %>%
  left_join(models_mad, by = c("a1", "a2")) %>%
  mutate(diff = dist.x - dist.y)
models_compare

best <- optim(c(0, 0), measure_distance_mad, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2]) +
  geom_smooth(method = "lm", se = FALSE)

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# 3. One challenge with performing numerical optimisation is that it’s only
# guaranteed to find one local optima. What’s the problem with optimising a 
# three parameter model like this?

model1 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}


# Any values where (a[1] + a[3]) == (a'[1] + a'[3]) have same fit.
# Essentially you can just combine the constant term.

# Visualizing Models ------------------------------------------------------
# Predictions

grid <- sim1 %>%
  data_grid(x)
grid

grid <- grid %>%
  add_predictions(sim1_mod)
grid

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(
    aes(y = pred),
    data = grid,
    color = "red",
    size = 1
  )

sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1

# Frequency polygon of residuals
ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

# Exercises 23.3.3 on website:
# http://r4ds.had.co.nz/model-basics.html#exercises-61
# 1. Instead of using lm() to fit a straight line, you can use loess() to fit 
# a smooth curve. Repeat the process of model fitting, grid generation, 
# predictions, and visualisation on sim1 using loess() instead of lm(). 
# How does the result compare to geom_smooth()?
sim1_mod_loess <- loess(y ~ x, data = sim1)
grid_loess <- sim1 %>%
  data_grid(x) %>%
  add_predictions(sim1_mod_loess)

sim1_loess <- sim1 %>%
  add_predictions(sim1_mod_loess) %>%
  add_residuals(sim1_mod_loess)

ggplot(sim1, aes(x, y)) +
  geom_point() +
  geom_smooth()

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(
    aes(y = pred),
    data = grid_loess,
    color = "red",
    size = 1
  )


# looks the same to me. In fact the message even says geom_smooth()
# uses method 'loess.'

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_point(data = sim1_loess, aes(sim1_loess$x, sim1_loess$resid),
             fill = "red", shape = 24)

# 2. add_predictions() is paired with gather_predictions() and 
# spread_predictions(). How do these three functions differ?

# add_prediction adds a single new column, .pred, to the input data. 
# spread_predictions adds one column for each model. gather_prections 
# adds two columns .model and .pred, and repeats the input rows for 
# each model.

df <- tibble::data_frame(
  x = sort(runif(100)),
  y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x))
)
df
plot(df)

m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))
grid %>% add_predictions(m1)

m2 <- lm(y ~ poly(x, 2), data = df)
grid %>% spread_predictions(m1, m2)
grid %>% gather_predictions(m1, m2)

# 3. What does geom_ref_line() do? What package does it come from? 
# Why is displaying a reference line in plots showing residuals useful and 
# important?

# Adds a reference line, defaults to double sized and white, 
# h for horizontal, v for vertical and it comes from modelr, even 
# though it works with a ggplot2 plot.

# Its important to see where the line x = 0, or h = 0 for code, is and to
# see empirically if the reference line is in the middle of the graph,
# implying the residuals have mean 0.

# 4. Why might you want to look at a frequency polygon of absolute residuals? 
# What are the pros and cons compared to looking at the raw residuals?

# Frequency polygon makes it easer to judge the distribution of residuals.
# From jrnold's solutions:
# The frequency polygon makes it easier to judge whether the variance and/or 
# absolute size of the residuals varies with respect to x. This is called 
# heteroskedasticity, and results in incorrect standard errors in inference. 
# In prediction, this provides insight into where the model is working well and
# where it is not. What is lost, is that since the absolute values are shown, 
# whether the model is over-predicting or underpredicting, or on average 
# correctly predicting in different regions of x cannot be determined.

# Formulas and Model Families ---------------------------------------------

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)

model_matrix(df, y ~ x1)
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)

# Categorical Variables
df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)

model_matrix(df, response ~ sex)

ggplot(sim2) +
  geom_point(aes(x, y))

mod2 <-  lm(y ~ x, data = sim2)

grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid,
    aes(y = pred),
    color = "red",
    size = 4
  )

# Interactions (Continuous and Categorical)
ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y~ x1 * x2, data = sim3)

grid <- sim3 %>%
  data_grid(x1, x2) %>% # finds all unique values of x1 and x2 and generates
  # all combinations
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

# Interactions (Two Continuous)
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)
grid

# Brief digression on seq_range()
# pretty:
seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)
# trim:
x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)
# expand:
x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)
# Back to model:

ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() + 
  facet_wrap(~ model) 

ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)

# Transformations
df <- tribble(
  ~y, ~x,
   1,  1,
   2,  2,
   3,  3
)

model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)

model_matrix(df, y ~ poly(x, 2))
# Outside range  of data, polynomials rapidly shoot off to positive or
# negative infinity. Alternative is to use natural spline from splines
# library:
model_matrix(df, y ~ ns(x, 2))

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)
sim5
ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)

# Exercises 23.4.5 on website:
# http://r4ds.had.co.nz/model-basics.html#exercises-62
# 1. What happens if you repeat the analysis of sim2 using a model without
# an intercept? What happens to the model equation? What happens to the 
# predictions?

# First lets look at original
mod2 <-  lm(y ~ x, data = sim2)

grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(
    data = grid,
    aes(y = pred),
    color = "red",
    size = 4
  )
  
# Now we drop the intercept
mod2_no_intcpt <-  lm(y ~ x - 1, data = sim2)

coef(mod2)
coef(mod2_no_intcpt)
summary(mod2)
summary(mod2_no_intcpt)
mod2
mod2_no_intcpt

grid_no_intcpt <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2_no_intcpt)
grid_no_intcpt

# The predictions are the same. In the 'categorical' case, by setting
# the intercept to 0, effectively we just 're-scale' the coefficients
# in the model. For example if x is 'd' then the model puts a 1 for x
# at x == d and 0 otherwise. So we multiply 1 by 0.7588142 and add 1.15
# in the regular model. This is the same as simply multiplying 1 by
# the sum (0.7588142 + 1.15 = 1.91), what we multiply by if the intercept
# is zero. With categorical data, its basically a contrast coding.

# 2. Use model_matrix() to explore the equations generated for the models
# I fit to sim3 and sim4. Why is * a good shorthand for interaction?


# 3. Using the basic principles, convert the formulas in the following 
# two models into functions. (Hint: start by converting the categorical 
# variable into 0-1 variables.)

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

# 4. For sim4, which of mod1 and mod2 is better? I think mod2 does a 
# slightly better job at removing patterns, but it’s pretty subtle. 
# Can you come up with a plot to support my claim?