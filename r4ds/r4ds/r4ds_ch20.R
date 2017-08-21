# R for Data Science Chapter 20
# Many Models with purr and broom
# Daniel J. Vera, Ph.D.
library(modelr)
library(tidyverse)
library(gapminder)

# gapminder ---------------------------------------------------------------
gapminder

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1 / 3)

nz <- filter(gapminder, country == "New Zealand")
nz %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full Data = ")
nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend + ")
nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")

# How do we fit this simple model to every country?
# Nested Data
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
by_country

by_country$data[[1]]

# List-Columns
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)

by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country

by_country %>%
  filter(continent == "Europe")
by_country %>%
  arrange(continent, country)

# Unnesting
by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

resids <- unnest(by_country, resids)
resids

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1/3) +
  facet_wrap(~continent)
broom::glance(nz_mod)

by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)

glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE)
glance

glance %>%
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()

# Exercises 25.2.5 on website:
# http://r4ds.had.co.nz/many-models.html#exercises-65
# 1. A linear trend seems to be slightly too simple for the overall trend. 
# Can you do better with a quadratic polynomial? How can you interpret the
# coefficients of the quadratic? (Hint you might want to transform year
# so that it has mean zero.)
country_model_poly <- function(df) {
  lm(lifeExp ~ poly(year - mean(year), 2), data = df)
}
by_country_poly <- gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, country_model)) %>%
  mutate(resids = map2(data, model, add_residuals))
by_country_poly

unnest(by_country_poly, resids) %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

by_country_poly %>% 
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

# 2. Explore other methods for visualising the distribution of R^2 per 
# continent. You might want to try the ggbeeswarm package, which provides
# similar methods for avoiding overlaps as jitter, but uses deterministic 
# methods.
library("ggbeeswarm")
by_country %>% 
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE) %>%
  ggplot(aes(continent, r.squared)) +
  geom_beeswarm()

# 3. To create the last plot (showing the data for the countries with the 
# worst model fits), we needed two steps: we created a data frame with one 
# row per country and then semi-joined it to the original dataset. It’s 
# possible avoid this join if we use unnest() instead of
# unnest(.drop = TRUE). How?