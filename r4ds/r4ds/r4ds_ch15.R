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

# Functions Are for Humans and Computers ===================================
# Name of function is important, should be short but clear what it does.
# Clarity is better than brevity for the name.
# Use verbs for function names, nouns for arguments.
# Too short
# f()
# Not a verg, or descriptive
# my_awesome_function()
# Long, but clear
# impute_missing()
# collapse_years()

# Good
# input_select()
# input_checkbox()
# input_text()

# Not so good
# select_input()
# checkbox_input()
# text_input()

# Exercises 19.3.1 on website:
# http://r4ds.had.co.nz/functions.html#exercises-48
# 1. Read the source code for each of the following three functions, puzzle
# out what they do, and then brainstorm better names.

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
# tests to see if string starts with prefix
has_prefix <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
has_prefix("unconditional", "un")

f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}
# checks if vector is >= 1 and if yes, returns vector without last
# component

drop_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
# replicates y up the length of x.
replicate <- function(x, y) {
  rep(y, length.out = length(x))
}
# 2. Take a function that you’ve written recently and spend 5 minutes 
# brainstorming a better name for it and its arguments.

# 3. Compare and contrast rnorm() and MASS::mvrnorm(). How could you make
# them more consistent?

#?rnorm
#?mvrnorm

# rnorm is the random generation function for the standard normal,
# default mean is 0 and standard deviation is 1. Normal density:
# f(x) = 1/(√(2 π) σ) e^-((x - μ)^2/(2 σ^2)).
# mvrnorm produces one or more samples from multivariate normal.
# In multivariate case, mu is now the vector of means, Sigma
# positive-definite symmetric covariance matrix, tol is the tolerance
# of numerical lack of positive definiteness in Sigma, emprical gives
# whether you are looking at population matrix. Matrix decomposition
# is done by eigendecomposition. Making them more consistent would mean
# the arguments have the same names but mu is a vector, not a scalar like 
# mean and Sigma is a matrix, not a scalar. Jrnold adds:
# "Both functions an internally consistent though; it would be bad to have 
# mu and sd or mean and Sigma."

# 4. Make a case for why norm_r(), norm_d() etc would be better than 
# rnorm(), dnorm(). Make a case for the opposite.

# If you start with norm_, then RStudio will help you find all norm
# functions so you don't have to memorize each one. The other way
# groups by 'action' instead of name though. E.g. rnorm and runif
# give random number generation from a normal and uniform distribution,
# respectively. similarly dnorm, dunif, pnorm, punif, qnorm, qunif
# or whatever distribution you are looking at.

# Conditional Execution ====================================================
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

has_name(islands)

if (c(TRUE, FALSE)) {
  print("Only the fist condition in vector evaluated!")
  } else {print("As you can see, this is not printed!")}

if (NA) {
  print("hi")
}

identical(0L, 0)
x <- sqrt(2) ^ 2
x == 2
x - 2
dplyr::near(x, 2)
# x == NA is useless! use is.na

# Multiple Conditions / Code Style
# switch()
# cut()

# # Good
# if (y < 0 && debug) {
#   message("Y is negative")
# }

# if (y == 0) {
#   log(x)
# } else {
#   y ^ x
# }

# # Bad
# if (y < 0 && debug)
# message("Y is negative")
# 
# if (y == 0) {
#   log(x)
# }
# else {
#   y ^ x
# }

# Can drop curly braces if you have very short if statement (one line):
y <- 10
x <- if (y < 20) "Too low" else "Too high"
# Full form is still easier to read:
if (y < 20) {
  x <- "Too low"
} else {
  x <- "Too high"
}

# Exercises 19.4.4 on website:
# http://r4ds.had.co.nz/functions.html#exercises-49
# 1. What’s the difference between if and ifelse()? Carefully read the help 
# and construct three examples that illustrate the key differences.

# the ifelse(test, yes, no) control flow statement returns returns a value
# with the same shape as test filled withe elements selected from
# either yes or no depending on whether the lement of test is TRUE or FALSE
x <- c(6:-4)
sqrt(x) # gives warning
ifelse(x >= 0, sqrt(x), NA) # gives warning
sqrt(ifelse(x >= 0, x, NA)) # no warning
# ifelse(a, b, c) is similar to
# if (a) {
#  b
# } else {
#   c
# }

# if tests single condition and ifelse tests each element.

# 2. Write a greeting function that says “good morning”, “good afternoon”, 
# or “good evening”, depending on the time of day. 
# (Hint: use a time argument that defaults to lubridate::now(). That will 
# make it easier to test your function.)
greeting <- function(time=lubridate::now()) {
  noon <- 12
  evening <- 18
  if (hour(time) < noon) {
    print("good morning")
  } else if (hour(time) < evening) {
    print("good afternoon")
  } else {
    print("good evening")
  }
}

# 3. Implement a fizzbuzz function. It takes a single number as input. 
# If the number is divisible by three, it returns “fizz”. If it’s divisible 
# by five it returns “buzz”. If it’s divisible by three and five, it returns 
# “fizzbuzz”. Otherwise, it returns the number. Make sure you first write 
# working code before you create the function.
fizzbuzz <- function(n) {
  if (n %% 3 == 0 && n %% 5 == 0) {
    print("fizzbuzz")
  } else if (n %% 3 == 0 && n %% 5 != 0) {
    print("fizz")
  } else if (n %% 3 != 0 && n %% 5 == 0) {
    print("buzz")
  } else {
    print(n)
  }
}

for (i in 1:100) {
  fizzbuzz(i)
} # text hasn't done for loops yet but this is a way to check.

# not in jrnold's solution, he adds a stopifnot(length(n) == 1) and
# stopifnot(is.numeric(n)), which I didn't at first, since the functions
# have not yet been covered but I think its better.

# 4. How could you use cut() to simplify this set of nested if-else
# statements?

temp <- 0 # just so temp refers to something
if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}

# using cut:
temp2 <- seq(-10, 50, by = 5)

cut(temp2, 
    breaks = c(-Inf, 10, 20, 30, Inf),
    labels =  c(
      "freezing",
      "cold",
      "warm",
      "hot"
      )
)
     
# How would you change the call to cut() if I’d used < instead of <=?
# What is the other chief advantage of cut() for this problem? 
# (Hint: what happens if you have many values in temp?)
cut(temp2, 
    breaks = c(-Inf, 10, 20, 30, Inf),
    labels =  c(
      "freezing",
      "cold",
      "warm",
      "hot"
    ),
    right = FALSE
)
# 5. What happens if you use switch() with numeric values?
arithmetic_stuff <- function(x, y, op) {
  switch(op,
         plus = x + y,
         minus = x - y,
         times = x * y,
         divide = x / y,
         stop("unknown op!")
         )
}
arithmetic_stuff(3, 7, "plus")
arithmetic_stuff(3, 7, "minus")
arithmetic_stuff(3, 7, "times")
arithmetic_stuff(3, 7, "divide")
arithmetic_stuff(3, 7, "exponentiation")
# using numbers makes switch select that number argument in the list.
arithmetic_stuff(3, 7, 3) # 3 should give times
arithmetic_stuff(3, 7, 1) # 1 should give plus



# 6. What does this switch() call do? What happens if x is “e”?

switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
# Experiment, then carefully read the documentation.

switch_call <- function(x) {
  switch(x, 
         a = ,
         b = "ab",
         c = ,
         d = "cd"
  )
}

switch_call("a")
switch_call("b")
switch_call("c")
switch_call("d")
switch_call("e")

# on the empty alternatives like a and c it the call skips to the next
# alternative e.g. if 'a' then 'b' will execute if 'c', then 'd'. 
# If 'e' is put in it returns NULL. From the help file:
# If there is a match then that element is evaluated unless it is
# missing, in which case the next non-missing element is evaluated,
# so for example switch("cc", a = 1, cc =, cd =, d = 2) evaluates to 2.
# If there is more than one match, the first matching element is used.
# In the case of no match, if there is a unnamed element of ... its value 
# is returned. (If there is more than one such argument an error is
# signaled.)


# Function Arguments =======================================================
# Compute confidence interval around mean using normal approximation.
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

# Good
mean(1:10, na.rm = TRUE)

# Bad
mean(x = 1:10, , FALSE)
mean(, TRUE, x = c(1:10, NA))

# Good
# average <- mean(feet / 12 + inches, na.rm = TRUE)

# Bad
# average<-mean(feet/12+inches,na.rm=TRUE)

# Choosing Names
# Some common short names:
# x, y, z: vectors
# w: a vector of weights
# df: a data frame
# i, j: numeric indices (typically rows and columns)
# n: length, number of rows
# p: number of columns

# Checking Values
wt_mean <- function(x, w) {
  sum(x * w) / sum(x)
}

wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}

wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

wt_mean(1:6, 1:3) # What does this mean? R 'recycles' so its same as
wt_mean(1:6, c(1:3, 1:3))

better_wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(x * w) / sum(x)
}
better_wt_mean(1:6, 1:3)

# Really verbose version:
wt_mean_v3 <- function(x, w, na.rm = FALSE) {
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be logical")
    }
  
  if (length(na.rm) != 1) {
    stop("`na.rm` must be length 1")
    } 
  
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
    }
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss] 
  } 
  sum(w * x) / sum(x)
}

# An alternative to above is to use stopifnot():
wt_mean_v4 <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss] 
  }
  sum(w * x) / sum(x)
}

wt_mean_v4(1:6, 6:1, na.rm = "foo")

# Dot-Dot-Dot
sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
stringr::str_c("a", "b", "c", "d", "e", "f")

# Taking multiple arguments relies on special argument: ...
# Examples:
commas <- function(...) {
  stringr::str_c(..., collapse = ", ")
}
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")

# BEWARE TYPOS!
x <- c(1, 2)
sum(x, na.mr = TRUE)

# Exercises 19.5.5 on website:
# http://r4ds.had.co.nz/functions.html#exercises-50
# 1. What does commas(letters, collapse = "-") do? Why?
commas(letters, collapse = "-")
# gives an error since its trying to run
# str_c(letters, collapse = "-", collapse = ", ")

# 2. It’d be nice if you could supply multiple characters to the pad
# argument, e.g. rule("Title", pad = "-+"). 
# Why doesn’t this currently work? How could you fix it?
rule2 <- function(..., pad = "-+") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule2("Important output")
# assumes 'pad' is only one character

rule3 <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  padchar <- nchar(pad)
  cat(title, " ",
      stringr::str_dup(pad, width %/% padchar),
      # if not multiple, fill in the remaining characters
      stringr::str_sub(pad, 1, width %% padchar),
      "\n", sep = "")
}
rule3("Important output", pad = "-+")

# 3. What does the trim argument to mean() do? When might you use it?
# trim defaults to zero and if zero computes the mean. If non-zero,
# it trims (from 0 to 0.5) a fraction of observations from each end of x
# then computes the mean. E.g.
x <- c(0:10, 50)
xm <- mean(x)
c(xm, mean(x, trim = 0.10))
# You would want to use this if you have a lot of outliers that may
# skew the mean too much.

# 4. The default value for the method argument to cor() is 
# c("pearson", "kendall", "spearman"). What does that mean? What value is 
# used by default?

# The method argument can take one of the three values: 'pearson',
# 'kendall', or 'spearman'. The default is 'kendall'.

# Return Values ============================================================
# Explicit Return Statements
complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  # Complicated code here
}

# f <- function() {
#   if (x) {
#     # Do 
#     # something
#     # that
#     # takes
#     # many
#     # lines
#     # to
#     # express
#   } else {
#     # return something short
#   }
# }

# Better to do early return for the simple case:

# f <- function() {
#   if(!x) {
#     return(something_short)
#   }
#   # Do 
#   # something
#   # that
#   # takes
#   # many
#   # lines
#   # to
#   # express 
# }

# Writing Pipeable Functions
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

show_missings(mtcars)
x <- show_missings(mtcars)
class(x)
dim(x)

mtcars %>%
  show_missings() %>%
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>%
  show_missings()

# Enviornment ==============================================================
f <- function(x) {
  x + y
}

y <- 100
f(10)

y <- 1000
f(10)
# From page 287 - 288 of text:
# In R, the above is valid because R uses lexical scoping rules to find a
# value associated with a name. Since y is not defined in the function
# definition, R will look in the ENVIORNMENT where it was defined. This
# behavior seems like a recipe for bugs, and indeed you should avoid 
# creating functions like this deliberately, but by and large it doesn’t 
# cause too many problems (especially if you regularly restart R to get to
# a clean slate). CTRL + SHIFT + F10 is your friend!

# Some devious things (it should go without saying but overriding addition
# is probably a bad idea, so I commented out the following but if you take
# the comments away, obsever the weirdness!):
# `+` <- function(x, y) {
#   if (runif( 1) < 0.1) { 
#     sum(x, y)
#     } else {
#       sum(x, y) * 1.1 } 
#   } 
# table(replicate(1000, 1 + 2))