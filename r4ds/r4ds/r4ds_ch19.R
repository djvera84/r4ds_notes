# R for Data Science Chapter 19
# Model Building
# Daniel J. Vera, Ph.D.
library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)
library(splines)

# Why Are Low-Quality Diamonds More Expensive? ----------------------------

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

ggplot(diamonds, aes(carat, price)) +
  geom_hex(bins = 50)

# Focus on diamonds < 2.5 carats, 99.7% of data) and log-transform
# the price and carat variables.
diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)
# seems linear

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

# examine residuals
diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)
# motivating plots with residuals

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

# A More Complicated Model
mod_diamond2 <- lm(
  lprice ~ lcarat + color + cut + clarity,
  data = diamonds2
)

grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamond2) %>%
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) +
  geom_point()

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)

diamonds2 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamond2) %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)

# Exercises 24.2.3 on website:
# http://r4ds.had.co.nz/model-building.html#exercises-63
# 1. In the plot of lcarat vs. lprice, there are some bright vertical strips. 
# What do they represent?
# The brighter the 'hexagon' the more instances at that particular point.
# The strips mean that there are more diamonds at those lcarat sizes.
# The most prominent are the lcarat of 0 (2 carat), -1/2 (1 carat), -1 (1/2 carat),
# etc. Basically human friendly fractions.

# 2. If log(price) = a_0 + a_1 * log(carat), what does that say about the 
# relationship between price and carat?
# Recall we used based 2. We exponentiate 2 by both sides of the equation to get
# price = 2^a_0 * carat^a_1, or basically
# price = a * carat^b where a and b are the paratmeters so that the relationship
# is exponential, i.e. price increases exponentially with the size. A 1%
# increase in carat is associated wtih a roughly a_1% increase in price.

# 3. Extract the diamonds that have very high and very low residuals. Is there 
# anything unusual about these diamonds? Are the particularly bad or good, or 
# do you think these are pricing errors?

# In text.

# 4. Does the final model, mod_diamonds2, do a good job of predicting diamond 
# prices? Would you trust it to tell you how much to spend if you were buying a 
# diamond?
diamonds2 %>%
  add_predictions(mod_diamond2) %>%
  add_residuals(mod_diamond2) %>%
  summarize(sq_err = sqrt(mean(resid^2)),
            abs_err = mean(abs(resid)),
            p975_err = quantile(resid, 0.975),
            p025_err = quantile(resid, 0.025),
            two_to_sq_err = 2^sq_err, # base 2 log
            two__to_abs_err = 2^abs_err,
            two_to_p975_err = 2^p975_err,
            two_to_p025_err = 2^p025_err)
# seems OK.

# What Affects the Number of Daily Flights? -------------------------------
daily <- flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarize(n = n())
daily
ggplot(daily, aes(date, n)) +
  geom_line()

daily <- daily %>%
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) +
  geom_boxplot()

# model
mod <- lm(n ~ wday, data = daily)

grid <- daily %>%
  data_grid(wday) %>%
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red", size = 4)

daily <- daily %>%
  add_residuals(mod)
daily %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line()

ggplot(daily, aes(date, resid, color = wday)) +
  geom_ref_line(h = 0) +
  geom_line()

daily %>%
  filter(resid < -100)

daily %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line(color = "grey50") +
  geom_smooth(se = FALSE, span = 0.20)

# Seasonal Saturday Effect
daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n)) +
    geom_point() +
    geom_line() +
    scale_x_date(
      NULL,
      date_breaks = "1 month",
      date_labels = "%b"
    )

# "term" variable
term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
      )
}
daily <- daily %>%
    mutate(term = term(date))
daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month",
    date_labels = "%b"
  )

daily %>%
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)
  
# overlay predictions from model onto raw data
grid <- daily %>%
  data_grid(wday, term) %>%
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") +
  facet_wrap(~term)

# use model robust to effect of outliers:
mod3 <- MASS::rlm(n~ wday * term, data = daily)
daily %>%
  add_residuals(mod3, "resid") %>%
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") +
  geom_line()

# Computed Variables
compute_vars <- function(data) {
  data %>%
    mutate(
      term = term(date),
      wday = wday(date, label = TRUE)
    )
}

# Time of Year: An alternative Approach
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod) %>%
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point()

# Exercises 24.2.3 on website:
# http://r4ds.had.co.nz/model-building.html#exercises-64
# 1. Use your Google sleuthing skills to brainstorm why there were fewer than 
# expected flights on Jan 20, May 26, and Sep 1. (Hint: they all have the same 
# explanation.) How would these days generalise to another year?

# Jan 20 is Sunday before MLK day. May 26 is Sunday before Memorial Day
# and Sep 1 is Sunday before Labor Day.

# 2. What do the three days with high positive residuals represent? How would 
# these days generalise to another year?

daily %>% 
  top_n(3, resid)
#> # A tibble: 3 × 5
#>         date     n  wday resid   term
#>       <date> <int> <ord> <dbl> <fctr>
#> 1 2013-11-30   857   Sat 112.4   fall
#> 2 2013-12-01   987   Sun  95.5   fall
#> 3 2013-12-28   814   Sat  69.4   fall

# High postive residual corresponds to under-estimating the number of flights.

# 3. Create a new variable that splits the wday variable into terms, but only 
# for Saturdays, i.e. it should have Thurs, Fri, but Sat-summer, Sat-spring, 
# Sat-fall. How does this model compare with the model with every combination 
# of wday and term?
daily <- daily %>%
  mutate(wday2 =
           case_when(.$wday == "Sat" & .$term == "summer" ~ "Sat-summer",
                     .$wday == "Sat" & .$term == "fall" ~ "Sat-fall",
                     .$wday == "Sat" & .$term == "spring" ~ "Sat-spring",
                     TRUE ~ as.character(.$wday)))
mod4 <- lm(n ~ wday2, data = daily)
daily %>%
  gather_residuals(sat_term = mod4, all_interact = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)

daily %>% 
  spread_residuals(sat_term = mod4, all_interact = mod2) %>%
  mutate(resid_diff = sat_term - all_interact) %>%
  ggplot(aes(date, resid_diff)) +
  geom_line(alpha = 0.75)

# 4. Create a new wday variable that combines the day of week, term (for Saturdays),
# and public holidays. What do the residuals of that model look like?

# 5. What happens if you fit a day of week effect that varies by month (i.e. 
# n ~ wday * month)? Why is this not very helpful?
# There are only 4-5 observations per parameter since only there are only 4-5 weekdays 
# in a given month.


# 6. What would you expect the model n ~ wday + ns(date, 5) to look like? 
# Knowing what you know about the data, why would you expect it to be not 
# particularly effective?

# Should have week cyclicality.

# 7. We hypothesised that people leaving on Sundays are more likely to be 
# business travellers who need to be somewhere on Monday. Explore that hypothesis 
# by seeing how it breaks down based on distance and time: if it’s true, you’d 
# expect to see more Sunday evening flights to places that are far away.
flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(dist_mean =  mean(distance),
            dist_median = median(distance)) %>%
  ggplot(aes(y = dist_mean, x = wday)) +
  geom_point()

flights %>% 
  mutate(date = make_date(year, month, day),
         wday = wday(date, label = TRUE)) %>%
  group_by(wday, hour) %>%
  summarise(dist_mean =  mean(distance),
            dist_median = median(distance)) %>%
  ggplot(aes(y = dist_mean, x = hour, colour = wday)) +
  geom_point() + 
  geom_line()

# 8. It’s a little frustrating that Sunday and Saturday are on separate ends of 
# the plot. Write a small function to set the levels of the factor so that the 
# week starts on Monday.
monday_first <- function(x) {
  forcats::fct_relevel(x, levels(x)[-1])
}

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(monday_first(wday), n)) + 
  geom_boxplot() +
  labs(x = "Day of Week", y = "Number of flights")