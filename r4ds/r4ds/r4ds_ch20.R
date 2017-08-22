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

# the last plot is made with the following code:
# step 1
# create a data frame with one row per country
glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = TRUE)
glance

bad_fit <- filter(glance, r.squared < 0.25)

# step 2 is perform the semi_join
gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()

# Dropping .drop:
glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)
glance
bad_fit2 <- filter(glance, r.squared < 0.25)
bad_fit2

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()
# It seems to just work the same. Not sure what the question is really
# asking.


# List-Columns ------------------------------------------------------------

data.frame(x = list(1:3, 3:5))
data.frame(
  x = I(list(1:3, 3:5)),
  y = c("1, 2", "3, 4, 5")
)

data.frame(
  x = I(list(1:3, 3:5)),
  y = c("1, 2", "3, 4, 5")
)

tibble(
  x = list(1:3, 3:5),
  y = c("1, 2", "3, 4, 5")
)

tribble(
   ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)

# Creating List-Columns ---------------------------------------------------
# With Nesting
# grouping
gapminder %>%
  group_by(country, continent) %>%
  nest()
# or specifying columns to be nested:
gapminder %>%
  nest(year:gdpPercap)

# From Vectorized Functions
df <- tribble(
  ~x1,
  "a,b,c",
  "d,e,f,g"
)

df %>%
  mutate(x2 = stringr::str_split(x1, ","))

df %>%
  mutate(x2 = stringr::str_split(x1, ",")) %>%
  unnest()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

# From Multivalued Summaries
mtcars %>%
  group_by(cyl) %>%
  summarize(q = quantile(mpg))

mtcars %>%
  group_by(cyl) %>%
  summarize(q = list(quantile(mpg)))

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>%
  group_by(cyl) %>%
  summarize(p = list(probs), q = list(quantile(mpg, probs))) %>%
  unnest()

# From a Named List
x <- list(
  a = 1:5,
  b = 3:4,
  c = 5:6
)
df <- enframe(x)

df %>%
  mutate(
    smry = map2_chr(
      name,
      value,
      ~ stringr::str_c(.x, ": ", .y[1])
    )
  )

# Exercises 25.4.5 on website:
# http://r4ds.had.co.nz/many-models.html#exercises-66
# 1. List all the functions that you can think of that take a atomic
# vector and return a list.

# 2. Brainstorm useful summary functions that, like quantile(), return 
# multiple values.
# range(), fivenum()
# 3. What’s missing in the following data frame? How does quantile()
# return that missing piece? Why isn’t that helpful here?

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg))) %>% 
  unnest()
#> # A tibble: 15 × 2
#>     cyl     q
#>   <dbl> <dbl>
#> 1     4  21.4
#> 2     4  22.8
#> 3     4  26.0
#> 4     4  30.4
#> 5     4  33.9
#> 6     6  17.8
#> # ... with 9 more rows
# the quantile values are missing (0%, 25%, etc.)
quantile(mtcars$mpg)
# note unnest drops the names of the vector
# 4. What does this code do? Why might might it be useful?

mtcars %>% 
  group_by(cyl) %>% 
  summarise_each(funs(list))

# It creates a data frame in which each row corresponds to a value of cyl,
# and each observation for each column (other than cyl) is a vector of all
# the values of that column for that value of cyl.

# Simplifying List-Columns ------------------------------------------------
# List to Vector
df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)

df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
)

df <- tribble( 
  ~ x, 
  list(a = 1, b = 2),
  list(a = 2, c = 4)
) 

df %>%
  mutate(a = map_dbl(x, "a"), 
         b = map_dbl(x, "b", .null = NA_real_)
  )

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y) 

# Ok because y and z have same number of elements in every row
df1 <- tribble(
  ~x, ~y,           ~z,
   1, c("a", "b"), 1:2,
   2, "c",           3
)
df1
df1 %>% unnest(y, z)

# Doesn't work because y and z have different number of elements
df2 <- tribble(
  ~x,  ~y,         ~z,
   1, "a",        1:2,
   2, c("b", "C"),  3
)
df2
df2 %>% unnest(y, z)

# Exercises 25.5.3 on website:
# http://r4ds.had.co.nz/many-models.html#exercises-67
# 1. Why might the lengths() function be useful for creating atomic
# vector columns from list-columns?
# From jrnold's solutions:
# The lengths() function gets the lengths of each element in a list.
# It could be useful for testing whether all elements in a list-column 
# are the same length. You could get the maximum length to determine how 
# many atomic vector columns to create.

# List the most common types of vector found in a data frame. What makes
# lists different?
# numeric, integer, factor, character, logical
# Lists are not atomic as they can contain other lists and other vectors
# The common types of vectors are atomic.