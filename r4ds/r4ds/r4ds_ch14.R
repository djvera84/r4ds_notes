# R for Data Science Chapter 14
# Pipes with magrittr
# Daniel J. Vera, Ph.D.
library(magrittr)

# Piping Alternatives ======================================================
# foo_foo <- little_bunny()

# Write same code in 4 ways:
# 1. Save each step as new object.
# 2. Overwrite original object many times.
# 3. Compose functions.
# 4. Use the pipe. %>%

# 1. Save each step as new object.
# foo_foo_1 <- hop(foo_foo, through = forest)
# foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
# foo_foo_3 <- bop(foo_foo_2, on = head)

diamonds2 <- diamonds %>%
  dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

diamonds$carat[1] <- NA
pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)

# 2. Overwrite the original
# foo_foo <- hop(foo_foo, through = forest)
# foo_foo <- scoop(foo_foo, up = field_mice)
# foo_foo <- bop(foo_foo, on = head)

# 3. Function composition
# bop(
#   scoop(
#     hop(foo_foo, through = forest),
#     up = field_mice
#   ),
#   on = head
# )

# 4. Use the Pipe
# foo_foo %>% 
#   hop(through = forest) %>%
#   scoop(up = field_mice) %>%
#   bop(on = head)

# my_pipe <- function(.) {
#  . <- hop(., through = forest) 
#  . <- scoop(., up = field_mice) 
# bop(., on = head) 
# } 
# my_pipe(foo_foo)

# The pipe will not work for two classes of functions:
# 1. functions that use the current enviornment.
assign("x", 10)
x
"x" %>% assign(100)
x
# If you do want to use assign with the pipe, you must be explicit 
# about the environment: 
env <- environment() 
"x" %>% assign(100, envir = env)

# get() and load() are other functions with this problem.

# 2. Functions that use 'lazy' evaluation.
tryCatch(stop("!"), error = function(e) "An error")
stop("!") %>%
  tryCatch(error = function(e) "An error")

# When Not to Use the Pipe =================================================
# Hadley Wickham recommends using something besides a pipe when there are 
# more than 10 steps in the pipe. Use variable names for intermediate
# steps; this helps with debugging and understanding of code.

# Mutliple inputs or outputs is also not the best situation for pipes.

# Directed graphs with complex dependency structure. Pipes are LINEAR!

# Other Tools from magrittr ================================================
# %T>% returns lefthand side instead of righthand side like %>%
rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()

rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

# %$% "explodes" :) variables out into a data frame so you can refer to
# them explicitly
mtcars %$%
  cor(disp, mpg)

# %<>% assigment operator
mtcars <- mtcars %>%
  transform(cyl = cyl * 2)
# same as
mtcars %<>% transform(cyl = cyl * 2)
# Hadley doesn't like this because its not as explicit as the first way.