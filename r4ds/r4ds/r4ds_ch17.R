# R for Data Science Chapter 17
# Iteration with purrr
# Daniel J. Vera, Ph.D.
library(tidyverse)

# For Loops ---------------------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Want medain of each column...well you COULD do it this way but WHY?
# i.e. copy and paste:
median(df$a)
median(df$b)
median(df$c)
median(df$d)

# Do it this way:
output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

y <- vector("double", 0)
seq_along(y)
1:length(y)

# Exercises 21.2.1 on website:
# http://r4ds.had.co.nz/iteration.html#exercises-55
# 1. Write for loops to:
#       a. Compute the mean of every column in mtcars.
mtcars
output <- vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}
output
#       b. Determine the type of each column in nycflights13::flights.
nycflights13::flights
output <- vector("character", ncol(nycflights13::flights))
for (i in seq_along(nycflights13::flights)) {
  output[[i]] <- typeof(nycflights13::flights[[i]])
}
output
#       c. Compute the number of unique values in each column of iris.
iris
output <- vector("double", ncol(iris))
for (i in seq_along(iris)) {
  output[[i]] <- sum(!duplicated(iris[[i]]))
}
output
#       d. Generate 10 random normals for each of μ = −10, 0, 10, and 100.
#          Think about the output, sequence, and body before you start 
#          writing the loop.
df <- tibble(c1 = vector("double", 10),
             c2 = vector("double", 10),
             c3 = vector("double", 10),
             c4 = vector("double", 10))
means <- c(-10, 0, 10, 100)
for (i in seq_along(df)) {
  df[i] <- rnorm(10, mean = means[i])
}
df
# 2. Eliminate the for loop in each of the following examples by taking 
# advantage of an existing function that works with vectors:
#   
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
# Answer:
out
letters
stringr::str_c(letters, collapse = "") 

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))

# Answer:
sd(x)
identical(sd, sd(x))

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}

# Answer
cumsum(x)
identical(out, cumsum(x))

# 3. Combine your function writing and for loop skills:
#       a. Write a for loop that prints() the lyrics to the children’s song 
#          “Alice the camel”.
#       b. Convert the nursery rhyme “ten in the bed” to a function. 
#          Generalise it to any number of people in any sleeping structure.
#       c. Convert the song “99 bottles of beer on the wall” to a function. 
#          Generalise to any number of any vessel containing any liquid on 
#          any surface.

# a.

# Alice the camel has five humps.
# Alice the camel has five humps.
# Alice the camel has five humps.
# So go, Alice, go.
# 
# Alice the camel has four humps.
# Alice the camel has four humps.
# Alice the camel has four humps.
# So go, Alice, go.
# 
# Alice the camel has three humps.
# Alice the camel has three humps.
# Alice the camel has three humps.
# So go, Alice, go.
# 
# Alice the camel has two humps.
# Alice the camel has two humps.
# Alice the camel has two humps.
# So go, Alice, go.
# 
# Alice the camel has one hump.
# Alice the camel has one hump.
# Alice the camel has one hump.
# So go, Alice, go.
# 
# Alice the camel has no humps.
# Alice the camel has no humps.
# Alice the camel has no humps.
# Now Alice is a horse

humps <- c("five", "four", "three", "two", "one", "no")
for (i in humps) {
  if (i == "one") {
    cat(str_c("Alice the camel has", rep(i, 3), "hump.\n", 
              sep = " ",
              collapse = "\n"), "\n")
  } else {
    cat(str_c("Alice the camel has", rep(i, 3), "humps.\n", 
              sep = " ",
              collapse = "\n"), "\n")
  }
  if (i == "no") {
    cat("Now Alice is a horse.\n")
  } else {
    cat("So go, Alice, go.\n")
  }
  cat("\n")
}

# b.
# There were ten in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and
# one fell out
# 
# There were nine in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over
# And one fell out
# 
# There were eight in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and one fell out
# 
# There were seven in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and one fell out
# 
# There were six in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and one fell out
# 
# There were five in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and one fell out
# 
# There were four in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and one fell out
# 
# There were three in the bed 
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and one fell out
# 
# There were two in the bed
# And the little one said,
# "Roll over! Roll over!"
# So they all rolled over and one fell out
# 
# There was one in the bed
# And the little one said,
# 
# "Alone at last!"

bed_rhyme <- function(sleepers = 10, structure = "bed") {
  for (i in sleepers:2) {
    cat(str_c("There were ", i, " in the ", structure,
              collapse = "\n"),
        "\nAnd the little one said.", 
        '\n"Roll over! Roll over!"', 
        "\nSo they all rolled over and one fell out\n\n"
        )
  }
  cat(str_c('There was one in the ', structure, "\n", 
      'And the little one said,\n',
      '"Alone at last!"', collpase = "\n"))
}

# c.

bottles_beer_or_other <- function(number = 99, vessel = "bottles", 
                                  liquid = "beer", surface = "wall" ) {
  for (i in number:1) {
    cat(str_c(i, " ", vessel, " of ", liquid, " on the ", surface, ",\n",
              i, " ", vessel, " of ", liquid, "\n",
              "Take one down, pass it around,", "\n",
              i - 1, " ", vessel, " of ", liquid, " on the ", surface, 
              "\n\n",
              collapse = "\n"))
    
  }
  cat("No", vessel, "of", liquid, "on the", surface)
}

bottles_beer_or_other()
bottles_beer_or_other(number = 12, 
                      vessel = "shots",
                      liquid = "tequila",
                      surface = "ceiling")

# 4. It’s common to see for loops that don’t preallocate the output and 
#    instead increase the length of a vector at each step:
# How does this affect performance? Design and execute an experiment.
output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output

# From jrnold's solutions
add_to_vector <- function(n) {
  output <- vector("integer", 0)
  for (i in seq_len(n)) {
    output <- c(output, i)
  }
  output
}

add_to_vector_2 <- function(n) {
  output <- vector("integer", n)
  for (i in seq_len(n)) {
    output[[i]] <- i
  }
  output
}
microbenchmark::microbenchmark(add_to_vector(10000), times = 3)
microbenchmark::microbenchmark(add_to_vector_2(10000), times = 3)

# For Loop Variations -----------------------------------------------------
# Modifying an existing object, instead of creating a new object
# Looping over names or values, instead of indices
# Handling outputs of unkown length
# Handling sequences of unknown length

# Modifying an Existing Object
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

df

# Now solve with for loop:
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

df

# Looping Patterns

# i. looping over the numeric indices with for (i in seq_along(xs)),
# and extracting the value with x[[i]].
# Loop over the elements: for (x in xs)
# # iii. loop over names: for (nm in names(xs)), access with x[[nm]]
# If you’re creating named output, make sure to name the results vector
# like so: 
results <- vector("list", length(x)) 
names(results) <- names(x)

# Iteration over the numeric indices is the most general form, because given
# the position you can extract both the name and the value: 
# for (i in seq_along(x)) { 
#   name <- names(x)[[i]] 
#   value <- x[[i]] 
#   }

# Unknown Output Length
means <- c(0, 1, 2)
output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

# Above is O(n^2). Better version is:
out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))

# Unkown Sequence Length (while loops)

# A while loop is simpler than a for loop because it only has two components,
# a condition and a body: 
#   while (condition) { 
#     # body 
#   } 
# A while loop is also more general than a for loop, because you can rewrite 
# any for loop as a while loop, but you can’t rewrite every while loop as a 
# for loop: 
#   for (i in seq_along( x)) {
#     # body 
#   } 
# # Equivalent to 
# i <- 1 
# while (i < = length(x)) { 
#   # body 
#   i <- i + 1 
#   }

# Example (three heads in a row):
flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips

# Exercises 21.3.5 on website:
# http://r4ds.had.co.nz/iteration.html#exercises-56
# 1. Imagine you have a directory full of CSV files that you want to
# read in. You have their paths in a vector, 
# files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), 
# and now want to read each one with read_csv(). 
# Write the for loop that will load them into a single data frame.
# df <- vector("list", length(files))
# for (file in seq_along(files)) {
#   df[[i]] <- read_csv(files[[i]])
# }
# df <- bind_rows(df)
# 2. What happens if you use for (nm in names(x)) and x has no names? 
# What if only some of the elements are named? 
# What if the names are not unique?
x <- 1:3
print(names(x))
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

# no names in vector so it runs zero iterations, i.e. skips the code
# in the for loop body.
# some names
x <- c(a = 1, 2, c = 3)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}
# you get an error.

# duplicates
x <- c(a = 1, a = 2, c = 3)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}
# only get first value of duplicated name!

# 3. Write a function that prints the mean of each numeric column in 
# a data frame, along with its name. For example, show_mean(iris) would
# print:
  
#  show_mean(iris)
#> Sepal.Length: 5.84
#> Sepal.Width:  3.06
#> Petal.Length: 3.76
#> Petal.Width:  1.20
# (Extra challenge: what function did I use to make sure that the 
# numbers lined up nicely, even though the variable names had different
# lengths?)

show_mean <- function(df, digits = 2) {
  maxstr <- max(stringr::str_length(names(df)))
  for (nm in names(df)) {
    if (is.numeric(df[[nm]])) {
      cat(stringr::str_c(stringr::str_pad(stringr::str_c(nm, ":"), 
                                          maxstr + 1L,
                                          side = "right"),
                format(mean(df[[nm]]), digits = digits, nsmall = digits),
                sep = " "),
          "\n")
    }
  }
}

show_mean(iris)

# 4. What does this code do? How does it work?

trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
mtcars

# code mutates the disp and am columns by calling the function
# trans[["disp"]] and trans[["am"]] on the column with the same
# respective names in mtcars.

# For Loops Versus Functionals --------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

# Compute mean of every column with a foor loop:
output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}

# Instead of
f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3
# a better way is this:
f <- function(x, i) abs(x - mean(x)) ^ i

# Same for above:
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  } 
  out
}

col_summary(df, median)
col_summary(df, mean)

# Exercises 21.4.1 on website:
# http://r4ds.had.co.nz/iteration.html#exercises-57
# 1. Read the documentation for apply(). In the 2d case, what two for loops
# does it generalise?
# It generalizes for loops over rows and columns.

# 2. Adapt col_summary() so that it only applies to numeric columns. You 
# might want to start with an is_numeric() function that returns a logical
# vector that has a TRUE corresponding to each numeric column.
col_summary_numeric <- function(df, fun) {
  # test if column is numeric
  numeric_cols <- vector("logical", length(df))
  for (i in seq_along(df)) {
    numeric_cols[[i]] <- is.numeric(df[[i]])
  } 
  indexes <- seq_along(df)[numeric_cols]
  n <- sum(numeric_cols)
  out <- vector("double", n)
  for (i in indexes) {
    out[i] <- fun(df[[i]])
  }
  out
}
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = letters[1:10],
  d = rnorm(10)
)
df
col_summary_numeric(df, mean)

# The Map Functions -------------------------------------------------------

# purrr functions
# map() makes a list
# map_lgl() makes a logical vector
# map_int() makes an integer vector
# map_dbl() makes a double vector
# map_chr() makes a character vector
# Makes life easier.
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

map_dbl(df, mean, trim = 0.5) # uses ...
z <- list(x = 1:3, y = 4:5)   # preserves names
map_int(z, length)

# Shortcuts
models <- mtcars %>%
  split(.$cyl) %>%
  map(function(df) lm(mpg ~ wt, data = df))
models
# anynomous function or 'lambda' function. Verbose in R so purrr does:
models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))

models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)

# or can use
models %>%
  map(summary) %>%
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)

# Base R
# lapply, sapply, vapply
x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
  )
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78),
  c(0.93, 0.21, 0.65, 0.13, 0.27),
  c(0.39, 0.01, 0.38, 0.87, 0.34)) 

threshold <- function(x, cutoff = 0.8) x[ x > cutoff] 
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

# Exercises 21.5.3 on website:
# http://r4ds.had.co.nz/iteration.html#exercises-58
# 1. Write code that uses one of the map functions to:
#   a. Compute the mean of every column in mtcars.
#   b. Determine the type of each column in nycflights13::flights.
#   c. Compute the number of unique values in each column of iris.
#   d. Generate 10 random normals for each of μ = −10, 0, 10 and 100.

# a.
map_dbl(mtcars, mean)
# b.
map_chr(nycflights13::flights, typeof)
map(nycflights13::flights, class)
# c.
map_int(iris, ~ sum(!duplicated(.)))
# jrnold:
map_int(iris, ~ length(unique(.)))
# d. 
map(c(-10, 0, 10, 100), rnorm, n = 10)

# 2. How can you create a single vector that for each column in a data 
# frame indicates whether or not it’s a factor?
# map_lgl(df, is.factor)
map_lgl(mtcars, is.factor)

# 3. What happens when you use the map functions on vectors that aren’t 
# lists? What does map(1:5, runif) do? Why?
map(1:5, runif)
# still returns a list of length equal to the input vector
# applies runif to each element of vector 1:5

# 4. What does map(-2:2, rnorm, n = 5) do? Why? 
# What does map_dbl(-2:2, rnorm, n = 5) do? Why?
map(-2:2, rnorm, n = 5) # makes samples n = 5 with means -2, 1, 0, 1, 2
# and returns a list wtih each element a numeric vector of length 5

map_dbl(-2:2, rnorm, n = 5)
# throws an error, expects a numeric vector of length one in each
# component, lists can have 'high dimensional' components.

# 5. Rewrite map(x, function(df) lm(mpg ~ wt, data = df)) to eliminate 
# the anonymous function.
map(list(mtcars), ~ lm(mpg ~ wt, data = .))

# Dealing with Failure ----------------------------------------------------

safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

# safely works with map
x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

# Making two lists
y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

# possibly() is another option:
x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

# quietly() also:
x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# Mapping Over Multiple Arguments -----------------------------------------

# map2() and pmap()

mu <- list(5, 10, -3)
mu %>%
  map(rnorm, n = 5) %>%
  str()

# what if we also want sd varying?
# First Method:
sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>%
  str()
# above intent is obfuscated so
# Better Method:
map2(mu, sigma, rnorm, n = 5) %>% str()

# Note that the arguments that vary for each call come before the function;
# arguments that are the same for every call come after.

# map2 is just a wrapper around a for loop:
map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

# Instead of map3(), map4(), map5() etc, purr provides pmap()

# Suppose want to vary the mean and sd as above but also the number of
# samples n:
n <- list(1, 2, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>%
  str()

# its better to name the arguments in case the positional matching is off
args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>%
  pmap(rnorm) %>%
  str()

# arguments in this case area all same length so we can store in 
# data frame:
params <- tribble(
  ~ mean, ~sd, ~n,
     5,     1,  1,
     10,    5,  3,
     -3,   10,  5
  )

params %>%
  pmap(rnorm)

# Invoking Different Functions

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()

# can use tribble again to make matching pairs a little more easy:
sim <- tribble( 
  ~ f,     ~ params, 
  "runif", list(min = -1, max = 1), 
  "rnorm", list(sd = 5), 
  "rpois", list(lambda = 10)
  )
sim %>% mutate(sim = invoke_map(f, params, n = 10))

# Walk --------------------------------------------------------------------

# Use walk when you want side effects rather than return value of function
# E.g. render files to screen or save files to disk.

x <- list(1, "a", 3)

x %>%
  walk(print)

# Also have walk2() and pwalk()
library(ggplot2)
plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

# Other Patterns of For Loops ---------------------------------------------
# Predicate Functions
iris %>%
  keep(is.factor) %>%
  str()

iris %>%
  discard(is.factor) %>%
  str()

x <- list(1:5, letters, list(10))
x %>%
  some(is_character)
x %>%
  every(is_vector)

x <- sample(10)
x

x %>% 
  detect(~ . > 5)
x %>%
  detect_index(~ . > 5)

x %>%
  head_while(~ . > 5)
x %>%
  tail_while(~ . > 5)

# Reduce and Accumulate
dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble( name = "Mary", treatment = "A")
)
dfs

dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)

x <- sample(10)
x
x %>% accumulate(`+`)

# Exercises 21.9.3 on website:
# http://r4ds.had.co.nz/iteration.html#exercises-59
# 1. Implement your own version of every() using a for loop. Compare it with 
# purrr::every(). What does purrr’s version do that your version doesn’t?
my_every <- function(.x, .f, ...) {
  for (i in .x) {
    if (!.f(i, ...)) {
      return(FALSE)
    }
  }
  TRUE
}

purrr::every

my_every(1:3, function(x) {x > 1})
my_every(1:3, function(x) {x > 0})

# From jrnold's solutions:
# The function purrr::every does fancy things with .p, like taking a logical
# vector instead of a function, or being able to test part of a string if 
# the elements of .x are lists.

# 2. Create an enhanced col_sum() that applies a summary function to every 
# numeric column in a data frame.

enhanced_col_summary <- function(df, fun, ...) {
  map(keep(df, is.numeric), fun, ...)
}

enhanced_col_summary(iris, mean)
enhanced_col_summary(mtcars, mean)

# 3. A possible base R equivalent of col_sum() is:
  
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  sapply(df_num, f)
  }
# But it has a number of bugs as illustrated with the following inputs:
  
df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
  )
# OK
col_sum3(df, mean)
# Has problems: don't always return numeric vector
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)
# What causes the bugs?

# sapply doesn't always return numeric vectors. From the documentation:
# sapply returns "if X has length zero or n = 0, an empty list."

sapply(df[0], is.numeric)
sapply(df[1], is.numeric)
sapply(df[1:2], is.numeric)
