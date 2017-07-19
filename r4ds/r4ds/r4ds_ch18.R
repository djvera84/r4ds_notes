# R for Data Science Chapter 18
# Model Basics with modelr
# Daniel J. Vera, Ph.D.
library(tidyverse)
library(modelr)
options(na.action = na.warn)

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
# http://r4ds.had.co.nz/iteration.html#exercises-60
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

# 3. One challenge with performing numerical optimisation is that it’s only
# guaranteed to find one local optima. What’s the problem with optimising a 
# three parameter model like this?

model1 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
  }