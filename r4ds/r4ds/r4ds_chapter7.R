# R for Data Science Chapter 7
# Tibbles with tibble
# Daniel J. Vera, Ph.D.
library(tidyverse)

as_tibble(iris)

tibble(
  x = 1:5,
  y = 1, 
  z = x ^ 2 + y
)

tb <- tibble(
  ':)' = "smile",
  ' ' = "space",
  '2000' = "number"
)

tribble(
  ~x, ~y, ~z,
  #--|--|----
 "a", 2, 3.6,
 "b", 1, 8.5
)

tibble( 
  a = lubridate:: now() + runif(1e3) * 86400, 
  b = lubridate:: today() + runif(1e3) * 30, 
  c = 1: 1e3, 
  d = runif(1e3), 
  e = sample(letters, 1e3, replace = TRUE)
)
        
nycflights13::flights %>%
  print(n = 10, width = Inf)

df <- tibble(
  x = runif(5), 
  y = rnorm(5)
  )

df$x
df[["x"]]
df[[1]]

df %>% .$x

# Exercises 10.5 on website: http://r4ds.had.co.nz/tibbles.html#exercises-18
# 1. How can you tell if an object is a tibble? 
# (Hint: try printing mtcars, which is a regular data frame).
# use the class() function
mtcars
class(mtcars)
class(as_tibble(mtcars))
# 2. Compare and contrast the following operations on a data.frame and 
# equivalent tibble. What is different? 
# Why might the default data frame behaviours cause you frustration?

df <- data.frame(abc = 1, xyz = "a")
df$x # can do partial matching! stores string as factor:
class(df$xyz)
df[, "xyz"]
df[, c("abc", "xyz")]

tb <- tibble('abc' = 1, 'xyz' = "a")
tb$x # no partial instead neads entire name
tb$xyz
class(tb$xyz)
tb[, "xyz"] #gives a tibble back
tb[, c("abc", "xyz")] # gives back a tibble

# 3. If you have the name of a variable stored in an object, e.g. 
var <- "mpg"

# how can you extract the reference variable from a tibble?
mtcars_tb <- as_tibble(mtcars)
mtcars_tb[[var]]

# You can use the double bracket, like df[[var]]. You cannot use 
# the dollar sign, becuase df$var would look for a column named var.

# 4. Practice referring to non-syntactic names in the following data frame 
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
# by:
  
  # Extracting the variable called 1.
annoying$`1`

  # Plotting a scatterplot of 1 vs 2.
ggplot(annoying, aes(`1`, `2`)) + 
  geom_point()
  # Creating a new column called 3 which is 2 divided by 1.
annoying %>%
  mutate(`3` = `2`/`1`)

annoying$`3` <- annoying$`2`/annoying$`1`

# Renaming the columns to one, two and three.

annoying <- rename(annoying, one = `1`, two = `2`, three = `3`)

# 5. What does tibble::enframe() do? When might you use it?

# A helper function that converts named atomic vectors or lists to 
# two-column data frames. For unnamed vectors, the natural sequence is 
# used as name column.

enframe(1:3)
enframe(c(a = 5, b = 7, c = 3))

# 6. What option controls how many additional column names are 
# printed at the footer of a tibble?

# n_extra option in print.tbl_df function. see:

package?tibble
