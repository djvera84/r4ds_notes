# R for Data Science Chapter 15
# Functions
# Daniel J. Vera, Ph.D.

# When Should You Write a Function? ========================================
df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) /
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) /
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) /
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) /
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

# Above rescales each column to have a range between 0 and 1.
# But it has a mistake! df$b, there is an a that should be b!

# Rewrite code using temperary variable with general name:
x <- df$a
(x - min(x, na.rm = TRUE)) /
  (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

# Turning above code into a function:
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))

# Three Key Steps to Function Creation are:
# 1. Pick a name (e.g. rescale01 which rescales a vector to lie in [0,1])
# 2. List the inputs (or arguments) to the function inside function.
# 3. Place code in body of function, a { block } follows.

# Check function:
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))
# http://r-pkgs.had.co.nz/tests.html

# Back to original example:
df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

# What if variable contains infinite values?
x <- c(1:10, Inf)
rescale01(x)

# Fix for rescale:
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

# Exercises 19.2.1 Practice on website:
# http://r4ds.had.co.nz/functions.html#practice-2
# 1. Why is TRUE not a parameter to rescale01()? What would happen if x 
# contained a single missing value, and na.rm was FALSE?

# na.rm is an argument for the range function and we are controlling for
# its behaviour within the function text. If it is set to FALSE, we get
# a vector of NA values.
rescale01_na_false <- function(x) {
  rng <- range(x, na.rm = FALSE)
  (x - rng[1]) / (rng[2] - rng[1])
}

rescale01_na_false(c(1, 2, 3, NA, 5))


# 2. In the second variant of rescale01(), infinite values are left unchanged. 
# Rewrite rescale01() so that -Inf is mapped to 0, and Inf is mapped to 1.
rescale01_Inf <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == -Inf] <- 0
  y[y == Inf] <- 1
  y
}

rescale01_Inf(c(Inf, 7, -Inf, NA, 1:3))

# 3. Practice turning the following code snippets into functions. Think about 
# what each function does. What would you call it? How many arguments does it 
# need? Can you rewrite it to be more expressive or less duplicative?

mean(is.na(x))
proportion_na <- function(x) {
  mean(is.na(x))
}

test <- c(1, 2, 3, NA, 5)
proportion_na(test)

x / sum(x, na.rm = TRUE)
proportion_component <- function(x) {
  x / sum(x, na.rm = TRUE)
}
proportion_component(test)


sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
coefficient_of_variation <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
coefficient_of_variation(test)

# 4. Follow http://nicercode.github.io/intro/writing-functions.html 
# to write your own functions to compute the variance and skew of a numeric 
# vector.
my_variance <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  sqr_err <- (x - m) ^ 2
  sum(sqr_err) / (n - 1)
}
my_variance(test)
my_variance(1:10)

my_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  m3 <- sum((x - m) ^ 3) / n
  s3 <- sqrt(my_variance(x)) ^ 3
  m3 / s3
}
my_skewness(test)
my_skewness(rgamma(10, 1, 1))
my_skewness(1:10)
my_skewness(c(1, 2, 5, 7))

# 5. Write both_na(), a function that takes two vectors of the same length and 
# returns the number of positions that have an NA in both vectors.
both_na <- function(x, y) {
  sum(is.na(x) & is.na(y))
}

both_na(c(NA, NA,  1, 2, NA, NA, 1), 
        c(NA,  1, NA, 2, NA, NA, 1))

both_na(c(NA, NA,  1, 2),
        c(NA,  1, NA, 2))
# 6. What do the following functions do? Why are they useful even though they 
# are so short?

is_directory <- function(x) file.info(x)$isdir
# checks whether the path in x is a directory
is_readable <- function(x) file.access(x, 4) == 0
# tests file for read permission

# both are useful since the name is much more explicit what code is doing.

# 7. Read the complete lyrics to “Little Bunny Foo Foo”. There’s a lot of 
# duplication in this song. Extend the initial piping example to recreate 
# the complete song, and use functions to reduce the duplication.

# Little bunny Foo Foo
# Went hopping through the forest
# Scooping up the field mice
# And bopping them on the head
# Down came the Good Fairy, and she said
# "Little bunny Foo Foo
# I don't want to see you
# Scooping up the field mice
# And bopping them on the head."
# I'll give you three chances,
# And if you don't behave, I will turn you into a goon!"
# And the next day...

# see https://jrnold.github.io/e4qf/functions.html


