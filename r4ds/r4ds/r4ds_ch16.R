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

x <- list(1, 2, 3)
x
str(x)
x_named <- list(a = 1, b = 2, c = 3)
x_named
str(x_named)
y <- list("a", 1L, 1.5, TRUE)
str(y)
z <- list(list(1, 2), list(3, 4))
str(z)

# Visualizing Lists
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

# Subsetting
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a
str(a)

# [ extracts a sublist. The results will always be a list.
a[1:2]
a[4]
str(a[1:2])
str(a[4])

# [[ extracts a single component from the list. It removes a level of
# hierarchy from the list:
y
str(y)
y[[1]]
y[[4]]
str(y[[1]])
str(y[[4]])

# $ is a shorthand for extracting named elements of a list. It works
# similar to [[ except that you don't need to use quotes:
a$a
a[["a"]]

a[[4]]
a[[4]][1]
a[[4]][[1]]
str(a[[4]])
str(a[[4]][1])
str(a[[4]][[1]])

# Exercises 20.5.4 on website:
# http://r4ds.had.co.nz/vectors.html#exercises-53 
# 1. Draw the following lists as nested sets:
#     a. list(a, b, list(c, d), list(e, f))
#     b. list(list(list(list(list(list(a))))))

# 2. What happens if you subset a tibble as if you’re subsetting a list?
df <- tibble(a = 1:3, b = 4:6, c = 7:9)
df
str(df)

df[1] # gives first column as n x 1 tibble (in this case n = 3)
str(df[1])

df[[2]] # gives the second column as a n dimensional vector (agian n = 3)
str(df[[2]])
df[[3]][1] # gives the entry on row 1, column 3 as 1-dim vector
str(df[[3]][1])

df[[3]][[1]] #same as above
str(df[[3]][1])

df$a # gives first column 'a' as n-dim vector.
df[["a"]]
df["a"]
# What are the key differences between a list and a tibble?
df2 <- tibble(a = 1:3, b = 4:6, c = c('a', 'b', 'c'))
df2
# tibbles can contain a mix of types but they are not 'recursive',
# i.e. all entries are 'scalar' types. You can't have a tibble
# with a tibble as an entry. From jrnold's solutions:

# Subsetting a tibble works the same way as a list; a data frame can be
# thought of as a list of columns. The key different between a list and 
# a tibble is that a tibble (data frame) has the restriction that all its 
# elements (columns) must have the same length.

# Attributes --------------------------------------------------------------

x <- 1:10
attr(x, "gretting")

attr(x, "gretting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)
str(attributes(x))

# There are three very important attributes that are used to implement
# fundamental parts of R: 
#  Names are used to name the elements of a vector.
#  Dimensions (dims, for short) make a vector behave like a matrix or array.
#  Class is used to implement the S3 object-oriented system.

as.Date
methods("as.Date")

getS3method("as.Date", "default")
getS3method("as.Date", "numeric")

# Augmented Vectors -------------------------------------------------------

# Factors
# Factors are built on top of integers and have a levels attribute:
x <- factor(c("ab", "cd", "ab", levels = c("ab", "cd", "ef")))
typeof(x)
attributes(x)
str(x)

# Dates and Date-Times
# Dates are numeric vectors that represent the number of days since
# 1 January 1970
x <- as.Date("1971-01-01")
unclass(x)

typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
typeof(y)
attributes(y)

# Tibbles
# Tibbles are augmented lists with three classes: tbl_df, tbl, data.frame
# Two attributes: column names and row names (names, row.names)
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)

# The main difference is the class. The class of tibble includes
# “data.frame,” which means tibbles inherit the regular data frame behavior 
# by default.

# The difference between a tibble or a data frame and a list is that all of
# the elements of a tibble or data frame must be vectors with the same 
# length. All functions that work with tibbles enforce this constraint.

# Exercises 20.7.4 on website:
# http://r4ds.had.co.nz/vectors.html#exercises-54 
# 1. What does hms::hms(3600) return? How does it print? What primitive type
# is the augmented vector built on top of? What attributes does it use?
hms::hms(3600)
# returns>01:00:00.
typeof(hms::hms(3600))
class(hms::hms(3600))
# primative is double
attributes(hms::hms(3600))
# units are seconds, classes are hms and difftime

# 2. Try and make a tibble that has columns with different lengths. 
# What happens?
tb <- tibble(x = 1:3, y = 1:2)
# you get an error, variables must be length 1 or 3 (max column dim).
# it also identifies the problem variable, y in this case.
# Note if we do make it 1:
tb <- tibble(x = 1:3, y = 1)
tb
# the y just repeats.
# 3. Based on the definition above, is it ok to have a list as a column of a
# tibble?
tibble(x = 1:3, y = list("a", 1, list(1:3))) # yes
# as long as dimensions of the vectors are the same, i.e. same length!