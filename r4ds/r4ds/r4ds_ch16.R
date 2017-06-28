# R for Data Science Chapter 16
# Vectors
# Daniel J. Vera, Ph.D.
library(tidyverse)


# Vector Basics -----------------------------------------------------------

# Two types of vectors:
# 1. Atomic vectors: logical, integer, double, character, complex, raw
# (integer and double vectors are collectively known as numeric)
# 2. Lists: sometimes called recursive vectors as they can contain other
# lists.

# Atomic vectors are homogeneous and lists can be heterogeneous.
# NULL is used to represent the absence of a vector as opposed to NA,
# which is used to represent the absence of a value in a vector. NULL
# behaves like a vector of length 0.

# Each vector has two key properties: type and length
typeof(letters)
typeof(1:10)

x <- list("a", "b", 1:10)
length(x)

# Vectors can contain metadata in the form of what are called attributes.
# Attributes are used to create augmented vectors, of which there are
# the following important types:
# 1. Factors, built on top of integer vectors
# 2. Dates and date-times, built on top of numeric vectors
# 3. Data frames and tibbles, built on top of lists.


# Important Types of Atomic Vector ----------------------------------------

# Logical, integer, double, and character are the four most important.
# Raw and complex are rarely used in data analysis.

# Logical
# Simplest take, take only three possible values: FALSE, TRUE, NA.
1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)

# Numeric (integer and double)
# In R, numbers are doubles by default. To make an integer, place L
# after the number.
typeof(1)
typeof(1L)
typeof(1.5L)

# Doubles are approximations.
x <- sqrt(2) ^ 2
x
x - 2

# Integers have one special value, NA
# Doubles have four: NA, NaN, Inf, and -Inf
c(-1, 0, 1) / 0
# use is.finite(), is.infinte(), and is.nan()  instead of == to check
# for these special values.

# Character
# Character vectors are the most complex type of atomic vector.
x <- "This is a reasonably long string."
pryr::object_size(x)
y <- rep(x, 1000)
pryr::object_size(y)

# y doesn’t take up 1000x as much memory as x, because each element of y is 
# just a pointer to that same string. A pointer is 8 bytes, so 1000 pointers 
# to a 136 B string is 8 * 1000 + 136 = 8.13 kB.

# Missing Values
NA            # logical
NA_integer_   # integer
NA_real_      # double
NA_character_ # character

# Exercises 20.3.5 on website:
# http://r4ds.had.co.nz/vectors.html#exercises-51
# 1. Describe the difference between is.finite(x) and !is.infinite(x).
# Lets look at it with a numeric (double vector):
x <- c(-Inf, 0, Inf, NA, NaN)
is.finite(x)
!is.infinite(x)
# is.finite only considers numbers to be finite. is.infinite only considers
# Inf and -Inf to be infinite.

# 2. Read the source code for dplyr::near() (Hint: to see the source code,
# drop the ()). How does it work?
# function (x, y, tol = .Machine$double.eps^0.5) 
# {
#   abs(x - y) < tol
# }
# <environment: namespace:dplyr>
# It looks like it examines the distance between x and y in terms of
# a specified tolerance to see if the distance is within that tolerance.
# The default tolerance is the square root of .Machine$double.eps, or
# 2.220446e-16, the smallest floating point number that the computer can
# represent.

# 3. A logical vector can take 3 possible values. How many possible values
# can an integer vector take? How many possible values can a double take? 
# Use google to do some research.
# ?.Machine
# R uses 32-bit integers and IEC 60559 floating-point arithmetic.
.Machine$double.xmin
.Machine$double.xmax
.Machine$integer.max
# Max integer is 2,147,483,647

# 4. Brainstorm at least four functions that allow you to convert a double
# to an integer. How do they differ? Be precise.
# truncating or rounding.

# 5. What functions from the readr package allow you to turn a string into
# logical, integer, and double vector?
# readr::parse_logical()
# readr::parse_integer()
# readr::parse_double()

# Using Atomic Vectors ----------------------------------------------------

# Coercion
# as.logical(), as.integer(), as.double(), as.character().
x <- sample(20, 100, replace = TRUE)
y <- x > 10
sum(y)  # how many are greater than 10?
mean(y) # what proportion are greater than 10?
typeof(c(TRUE, 1L)) # most complex type wins
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))
# An atomic vector cannot have a mix of different types becaues the type
# is a property of the complete vector, not the individual elements.

# Test Functions
# is_* functions from purr are safer than base R is.*
# scalar version: is_scalar_*

# Scalars and Recycling Rules

# Shorter vector is repeated to the same length as longer vector.
# There are no "scalars" in R; i.e. a scalar is just a vector of length 1.
# Most functions are 'vectorized.'

sample(10) + 100
runif(10) > 0.5

# In R, basic mathematical operations work with vectors. That means that you
# should never need to perform explicit iteration when performing simple
# mathematical computations.

1:10 + 1:2
1:10 + 1:3

tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

# Naming Vectors
c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))

# Subsetting
# With integers:
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1, 1, 5, 5, 5, 2)]
x[c(-1, -3, -5)]
x[c(1, -1)]
x[0]
# With logical expressions (comparison)
x <- c(10, 3, NA, 5, 8, 1, NA)
# All non-missing values of x
x[!is.na(x)]
# All even (or missing!) values of x
x[x %% 2 == 0]
# With names:
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

# Exercises 20.4.6 on website:
# http://r4ds.had.co.nz/vectors.html#exercises-52 
# 1. What does mean(is.na(x)) tell you about a vector x? What about
# sum(!is.finite(x))?
x <- c(NA, NaN, 3, 4, 5, Inf, 0, -1, 2, 10)
mean(is.na(x)) 
# the proportion of the components of the vector that are NA
# in above, .2
sum(!is.finite(x))
# the number of compoents that are not finite, in the above 3

# 2. Carefully read the documentation of is.vector(). What does it 
# actually test for? Why does is.atomic() not agree with the definition
# of atomic vectors above?
# is.vector tests if x is a vector of specified mode having no attributes
# other than names.
# you can have atomic types that are not vectors in is.atomic.

# 3. Compare and contrast setNames() with purrr::set_names().
setNames
purrr::set_names
# 4. Create functions that take a vector as input and returns:
  
#       a. The last value. Should you use [ or [[?
last_value <- function(x) {
  x[[length(x)]]
}
#       b. The elements at even numbered positions.
even_positions <- function(x) {
  x[seq_along(x) %% 2 == 0]
}
#       c. Every element except the last value.
drop_last <- function(x) {
  x[-length(x)]
}
#       d. Only even numbers (and no missing values).
even_numbers <- function(x) {
  x[!is.na(x) & (x %% 2 == 0)]
}
                                        
# 5. Why is x[-which(x > 0)] not the same as x[x <= 0]?
x                                        
x[-which(x > 0)]
x[x <= 0]
# treat missing values, NA and NaN differently.

# 6. What happens when you subset with a positive integer that’s bigger than
# the length of the vector? What happens when you subset with a name that 
# doesn’t exist?
x[11]
y <- 1:7
y[12]
# you get NA

x <- c(abc = 1, def = 2, xyz = 5)
x[["xyz"]]
x[["hij"]]
# you get an error: "subscript out of bounds"

# Recursive Vectors (Lists) -----------------------------------------------

