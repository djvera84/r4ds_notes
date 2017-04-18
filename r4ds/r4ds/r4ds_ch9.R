# R for Data Science Chapter 9
# Tidy Data with tidyr
# Daniel J. Vera, Ph.D.
library(tidyverse)

table1
table2
table3
table4a
table4b

# Compute rate per 10,000
table1 %>%
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>%
  count(year, wt = cases)

# Visualize changes over time
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country)) # only two years of data

# Exercises 12.2.1 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-21

# 1. Using prose, describe how the variables and observations are organised 
# in each of the sample tables.

# First table 1, the tidy data:
table1
# Each variable - country, year, cases, population, is organized by column,
# with each observation on a row and value of a variable for an observation
# in a cell.

# Now table 2:
table2
# Table 2 has the country and year variable in their own column but breaks
# up a single observation across two rows with the third column standing for
# which of the two varialbes of population or cases the row signifies and
# a count column giving the value of either the cases or population variable
# given in the type column. 

# Table 3:
table3
# Table 3 is almost as table1 however the variables cases and population have
# been munged into a new varialbe "rate" in column 3 that is now a string;
# we note that we would have to parse (actually tidyr has a function that can
# handle this called separate, see below) the string to recover the cases and
# population. A better way would be to use dplyr::mutate to create rate from
# table1

# Table 4 is split across two tables...yuk:
table4a
table4b
# The a table gives the cases with the column variables now being years and
# table b gives population instead of cases. 


# 2. Compute the rate for table2, and table4a + table4b. You will need to 
# perform four operations:
#     1. Extract the number of TB cases per country per year.
#     2. Extract the matching population per country per year.
#     3. Divide cases by population, and multiply by 10000.
#     4. Store back in the appropriate place.
# Which representation is easiest to work with? Which is hardest? Why?

# First table2.
table2

# Get the cases out (Extract the number of TB cases per country per year.)
(table2_cases <- table2 %>%
  filter(type == "cases"))

# Get the population out 
# (Extract the matching population per country per year.)
(table2_population <- table2 %>%
    filter(type == "population"))

# Now combine them:
(table2_mod <- tibble(
  country = table2_cases$country,
  year = table2_cases$year,
  cases = table2_cases$count,
  population = table2_population$count
  ))

# CHECK!
table1 == table2_mod
# see section on tidyr::spread for an alternative to deal with the above

# Now compute rate and append to table:
# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.

(table2_mod <- table2_mod %>%
    mutate(rate = (cases / population) * 10000))

# Now table4a and table4b.
table4a
table4b

# First I combine the tables and create a variable for year,
# cases, and population:

(table4_mod = tibble(
  country = c(table4a$country, table4b$country),
  year = c(rep(colnames(table4a)[2],length(table4a$`1999`)),
           rep(colnames(table4a)[3],length(table4a$`2000`))),
  cases = c(table4a$`1999`, table4a$`2000`),     #4a has cases
  population = c(table4b$`1999`, table4b$`2000`) #4b has population
))
# see section on tidyr::gather for an alternative to deal with the above

# Then I calcualte the rate and store it and then arrange to match
# the other tables:
(table4_mod <- table4_mod %>%
    mutate(rate = (cases / population) * 10000) %>%
    arrange(country))


# 3. Recreate the plot showing change in cases over time using table2 
# instead of table1. What do you need to do first?

# First recall:
table1
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country)) # only two years of data

# We did all the work to get table2 modified to table1 so we can just 
# call ggplot on table2_mod now since the tables are identical:
ggplot(table2_mod, aes(year, cases)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country)) # only two years of data

# but suppose we HAD to start wwith table2. Then we just need to look at 
# cases:
table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(year, count)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country)) + # only two years of data
  labs(y = "cases")

# tidyr::gather=============================================================
table4a
table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

table4b
table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")

tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)
# tidyr::spread=============================================================
table2
spread(table2, key = type, value = count)

# Exercises 12.3.3 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-22

# 1. Why are gather() and spread() not perfectly symmetrical?
# Carefully consider the following example:
stocks <- tibble(
    year = c(2015, 2015, 2016, 2016),
    half = c(1, 2, 1, 2),
    return = c(1.88, 0.59, 0.92, 0.17)
  )

stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)

# (Hint: look at the variable types and think about column names.)
# Both spread() and gather() have a convert argument. What does it do?

# spread and gather are not perfectly symetricall because column
# type is not transfered between them. Year is a <dbl> in stocks then
# we spread it and it is a variable name <chr>, then we gather it.

# convert: If TRUE will automatically run type.convert on the key column. 
# This is useful if the column names are actually numeric, integer, or 
# logical.


# 2. Why does this code fail?

table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")
#> Error in combine_vars(vars, ind_list): 
#> Position must be between 0 and n
# The first arguments do not have backticks ` so R sees numbers, this
# should work:
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

# 3. Why does spreading this tibble fail? How could you add a new column 
# to fix the problem?

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people %>% 
  spread(key, value)
# there are duplicates, e.g. there are two Philip Woods age entries
# row 1 and row 3. We can add an "id" column.
people_mod <- tribble(
  ~name,             ~key,    ~value, ~id, 
  #-----------------|--------|------|-----
  "Phillip Woods",   "age",       45, 1,
  "Phillip Woods",   "height",   186, 1,
  "Phillip Woods",   "age",       50, 2,
  "Jessica Cordero", "age",       37, 3,
  "Jessica Cordero", "height",   156, 3
)
people_mod %>% 
  spread(key, value)
# 4. Tidy the simple tibble below. Do you need to spread or gather it? 
# What are the variables?

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

# the variables are sex and if pregnant. we need to gather these:
preg %>%
  gather(male, female, key = "sex", value = "count")

# tidyr::seperate===========================================================
table3
table3 %>%
  separate(rate, into = c("cases", "population"))
# OR
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/")
# not column types in above, to fix we can convert:
table3 %>%
  separate(
    rate,
    into = c("cases", "population"), 
    convert = TRUE
  )
    
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)
# tidyr::unite==============================================================
table5
table5 %>%
  unite(new, century, year)

table5 %>%
  unite(new, century, year, sep = "")
# Exercises 12.2.3 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-23

# 1. What do the extra and fill arguments do in separate()? Experiment with 
# the various options for the following two toy datasets.

# extra: 
# If sep is a character vector, this controls what happens when 
# there are too many pieces. There are three valid options:
# "warn" (the default): emit a warning and drop extra values.
# "drop": drop any extra values without a warning.
# "merge": only splits at most length(into) times

# fill:	
# If sep is a character vector, this controls what happens when 
# there are not enough pieces. There are three valid options:
# "warn" (the default): emit a warning and fill from the right
# "right": fill with missing values on the right
# "left": fill with missing values on the left
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "warn")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "drop")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "warn")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "right")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "left")

# 2. Both unite() and separate() have a remove argument. What does it do? 
# Why would you set it to FALSE?

# ?unite
# If TRUE removes input columns from output data frame, you would want it
# FALSE if you wanted to see orignal variable from original column.


# Compare and contrast separate() and extract(). Why are there three 
# variations of separation (by position, by separator, and with groups), 
# but only one unite?

# ?extract
# extract uses regular expression with 'capturing groups' and turns each
# group into a new column. from jrnold's github:
# In unite it is unambigous since it is many columns to one, and once the 
# columns are specified, there is only one way to do it, the only choice is 
# the sep. In separate, it is one to many, and there are multiple ways to 
# split the character string.

# Missing Values============================================================
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks %>%
  spread(year, return) %>%
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks %>% 
  complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~ response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment %>%
  fill(person)

# Exercises 12.3.3 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-24

# 1. Compare and contrast the fill arguments to spread() and complete().
# The fill argument in spread, if set, replaces missing values with the value
# specified in the fill argument, default is NA.
# The fill argument in complete also gives the value to be replaced however
# it is specified by a list that gives a single value for each variable 
# intead of NA.
df <- data_frame(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  item_name = c("a", "b", "b"),
  value1 = 1:3,
  value2 = 4:6)

df %>% 
  complete(group, nesting(item_id, item_name))

# You can also choose to fill in missing values
df %>% 
  complete(group, nesting(item_id, item_name), fill = list(value1 = 0))

# 2. What does the direction argument to fill() do?
# direction tells the function to fill either down the entries or up the
# entries. Default is down.

# Case Study ===============================================================
tidyr::who
View(who)

who1 <- who %>%
  gather(
    new_sp_m014:newrel_f65, key = "key",
    value = "cases",
    na.rm = TRUE
  )

View(who1)

who1 %>%
  count(key)

who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")

who3 %>% 
  count(new)

who4 <- who3 %>%
  select(-new, -iso2, -iso3)

who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)

# complete pipe:
tidy_who <- who %>%
  gather(code, cases, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(
    code = stringr::str_replace(code, "newrel", "new_rel")
  ) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)
View(tidy_who)

# Exercises 12.3.3 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-25

# 1. In this case study I set na.rm = TRUE just to make it easier to check 
# that we had the correct values. Is this reasonable? Think about how 
# missing values are represented in this dataset. 
# Are there implicit missing values? Whatâs the difference between an NA 
# and zero?

# 0 means there are no cases as opposed to missing data. From jrnold's
# solutions, jrnold checked as follows
gather(who, new_sp_m014:newrel_f65, key = "key", value = "cases") %>%
  group_by(country, year)  %>%
  mutate(missing = is.na(cases)) %>%
  select(country, year, missing) %>%
  distinct() %>%
  group_by(country, year) %>%
  filter(n() > 1)

# 2. What happens if you neglect the mutate() step? 
# (mutate(key = stringr::str_replace(key, "newrel", "new_rel")))
who_bad_key <- who %>%
  gather(code, cases, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

# we cannot deterime values associated with the newrel observations, 
# several sex and age values are missing.

# 3. I claimed that iso2 and iso3 were redundant with country. Confirm 
# this claim.

who %>%
  count(country, iso2, iso3)

# or

who3 %>%
  select(country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  filter(n() > 1)

# 4. For each country, year, and sex compute the total number of cases of 
# TB. Make an informative visualisation of the data.

tidy_who %>%
  group_by(country, year, sex) %>%
  count(wt = cases) %>%
  ggplot(aes(year, n)) +
  geom_line(aes(group = country, color = sex))

# since above doesnt have a lot of data points prior to 1995, we filter:
tidy_who %>%
  group_by(country, year, sex) %>%
  filter(year > 1995) %>%
  count(wt = cases) %>%
  ggplot(aes(year, n)) +
  geom_line(aes(group = country, color = sex))

# we can also facet by male and female but is hard to seperate countries:
tidy_who %>%
  group_by(country, year, sex) %>%
  filter(year > 1995) %>%
  count(wt = cases) %>%
  ggplot(aes(year, n)) +
  geom_line(aes(group = country)) +
  facet_wrap(~ sex, nrow = 2)

# we can focus on countries with a large number of cases:
tidy_who %>%
  group_by(country) %>%
  summarize(cases_per_country = sum(cases)) %>%
  arrange(desc(cases_per_country))
# above shows that the top 10 countries have over 905,000 cases so
# we filter to focus on these:
tidy_who %>%
  group_by(country) %>%
  mutate(cases_per_country = sum(cases)) %>%
  group_by(country, year, sex) %>%
  filter(cases_per_country > 905000, year > 1995 ) %>%
  count(wt = cases) %>%
  ggplot(aes(year, n)) +
  geom_line(aes(color = country)) +
  facet_wrap(~ sex, nrow = 2)
  
# easier to read if we only have top 5 countries:
tidy_who %>%
  group_by(country) %>%
  mutate(cases_per_country = sum(cases)) %>%
  group_by(country, year, sex) %>%
  filter(cases_per_country > 1000000, year > 1995 ) %>%
  count(wt = cases) %>%
  ggplot(aes(year, n)) +
  geom_line(aes(color = country)) +
  facet_wrap(~ sex, nrow = 2)
