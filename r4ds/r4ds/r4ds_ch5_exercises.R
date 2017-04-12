# R Chapter 5 Exercises Only
# I have the notes to the chapter in another script, 
# "R for Data Science Chapter 5.R"
# For all the exercises we need the following.
library(tidyverse)
library(nycflights13)
library(ggstance)
library(lvplot)
library(ggbeeswarm)
library(hexbin)

# Some exercises ask you to install some of the above packages but this
# is already done so I just load them above. I also use the ggplot
# call convetion described at end of chapter, ommitting explicit naming
# of first variables in funciton calls for graphs.

# 7.3.4 on http://r4ds.had.co.nz/exploratory-data-analysis.html
# 7.3.4=====================================================================
# 1. Explore the distribution of each of the x, y, and z variables in 
# diamonds. What do you learn? Think about a diamond and how you might 
# decide which dimension is the length, width, and depth.
ggplot(diamonds, aes(x)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(y)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(z)) +
  geom_histogram(binwidth = 0.5)

# 2. Explore the distribution of price. Do you discover anything unusual 
# or surprising? (Hint: Carefully think about the binwidth and make sure 
# you try a wide range of values.)

ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 100) 

ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 500)

ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 1000)

ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(0, 1500))

# 3. How many diamonds are 0.99 carat? How many are 1 carat? What do you 
# think is the cause of the difference?
ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.01) + 
  coord_cartesian(xlim = c(0.95, 1.05), ylim = c(0, 100))

diamonds %>% 
  filter(carat == 0.99) %>% 
  count()

diamonds %>% 
  filter(carat == 1) %>% 
  count()
# There are 23 0.99 carats, relatively low, compared to 1558 for 1 carrat.
# Prospective buyers if willing to already buy 0.99 may see that they
# may as well purchase the full 1 carat. Compare average prices:

diamonds %>% 
  filter(carat == 0.99) %>% 
  summarize(mean(price, na.rm = TRUE))

diamonds %>% 
  filter(carat == 1) %>% 
  summarize(mean(price, na.rm = TRUE))

# Average price for 0.99 carat is $4400 and averate for 1 carat is $5240, 
# roughly 20% higher.

# 4. Compare and contrast coord_cartesian() vs xlim() or ylim() when zooming
# in on a histogram. What happens if you leave binwidth unset? What happens 
# if you try and zoom so only half a bar shows?
ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(0, 1500))

ggplot(diamonds, aes(price)) +
  geom_histogram(binwidth = 100) +
  xlim(0, 5000) + ylim(0, 1500)

# coord_cartesian simply zooms in on the area specified by the limits. 
# The calculation of the histogram is unaffected. However, the xlim and ylim
# functions first drop any values outside the limits. 

# 7.4.1=====================================================================
# 1. What happens to missing values in a histogram? What happens to missing 
# values in a bar chart? Why is there a difference?
# In histogram, they are ommitted, where would be put them?
# In bar chart, a missing value 'NA' category is created.
ggplot(data = flights, mapping = aes(x = dep_delay)) +
  geom_histogram()

flights %>%
  mutate(carrier = ifelse(carrier == "AA", NA, carrier)) %>%
  ggplot(aes(carrier)) +
  geom_bar()

# 2. What does na.rm = TRUE do in mean() and sum()?
# Removes the missing values from the calculation.

# 7.5.1.1===================================================================
# 1. Use what you’ve learned to improve the visualisation of the departure 
# times of cancelled vs. non-cancelled flights.
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), size = 1, binwidth = 1/4)

# better yet, a boxplot:
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot() + 
  geom_boxplot(aes(cancelled, sched_dep_time))

# 2. What variable in the diamonds dataset is most important for predicting 
# the price of a diamond? How is that variable correlated with cut? 
# Why does the combination of those two relationships lead to lower quality
# diamonds being more expensive?
lm_diamonds <- lm(price ~ ., data = diamonds)
summary(lm_diamonds)

diamonds_numeric <- diamonds[-c(2, 3, 4)]
cor(diamonds_numeric)

lm_diamonds_carat = lm(price ~ carat, data = diamonds)
summary(lm_diamonds_carat)

# carat looks like the most highly correlated.
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

ggplot(diamonds,aes(cut, carat)) +
  geom_boxplot()

# as cut quality goes up, it looks like on average carat size goes down.

# 3. Install the ggstance package, and create a horizontal boxplot. How does 
# this compare to using coord_flip()?
ggplot(diamonds,aes(carat, cut)) +
  geom_boxploth()

ggplot(diamonds,aes(cut, carat)) +
  geom_boxplot() +
  coord_flip()

# you get the same graph but with coord_flip, you map the x and y and flip
# with geom_boxploth you are graphing 'horizontally' so the first aesthetic
# is the actual x (which would be y in the usual version) and vice versa.

# 4. One problem with boxplots is that they were developed in an era of much
# smaller datasets and tend to display a prohibitively large number of 
# “outlying values”. One approach to remedy this problem is the letter value
# plot. Install the lvplot package, and try using geom_lv() to display the 
# distribution of price vs cut. What do you learn? How do you interpret the 
# plots?

ggplot(diamonds, aes(cut, price)) +
  geom_lv(color = "gold")

ggplot(diamonds, aes(cut, price)) +
  geom_boxplot()

ggplot(diamonds, aes(cut, price)) +
  geom_lv(color = "gold") +
  coord_flip()
# it appears that for the 'fair' cuts as the price increases there are less 
# data points but there are more data points than originally thought with
# the boxplot.

# 5. Compare and contrast geom_violin() with a facetted geom_histogram(), or a 
# coloured geom_freqpoly(). What are the pros and cons of each method?
ggplot(diamonds, aes(cut, price)) +
  geom_violin() +
  coord_flip()

ggplot(diamonds, aes(price)) +
  geom_histogram() +
  facet_wrap(~ cut, ncol = 1)

ggplot(diamonds, aes(price, ..density..)) +
  geom_freqpoly(aes(color = cut)) 
  
# from jrnold's github:
# I produce plots for these three methods below. The geom_freqpoly is better 
# for look-up: meaning that given a price, it is easy to tell which cut has 
# the highest density. However, the overlapping lines makes it difficult to
# distinguish how the overall distributions relate to each other. The 
# geom_violin and facetted geom_histogram have similar strengths and 
# weaknesses. It is easy to visually distinguish differences in the overall
# shape of the distributions (skewness, central values, variance, etc). 
# However, since we can’t easily compare the vertical values of the 
# distribution, its difficult to look up which category has the highest 
# density for a given price. All of these methods depend on tuning 
# parameters to determine the level of smoothness of the distribution.

# 6. If you have a small dataset, it’s sometimes useful to use geom_jitter()
# to see the relationship between a continuous and categorical variable. 
# The ggbeeswarm package provides a number of methods similar to 
# geom_jitter(). List them and briefly describe what each one does.
package?ggbeeswarm
# ggbeeswarm provides two different methods to create beeswarm-style plots 
# using ggplot2. It does this by adding two new ggplot geom objects:
  
#  geom_quasirandom: Uses a van der Corput sequence or Tukey texturing 
# to space the dots to avoid overplotting. This uses sherrillmix/vipor.
ggplot(data = mpg) +
  geom_quasirandom(mapping = aes(x = reorder(class, hwy, FUN = median),
                                 y = hwy))

# geom_beeswarm: Uses the beeswarm library to do point-size based offset.

ggplot(data = mpg) +
  geom_beeswarm(mapping = aes(x = reorder(class, hwy, FUN = median),
                              y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = reorder(class, hwy, FUN = median),
                              y = hwy))
# 7.5.2.1===================================================================
# 1. How could you rescale the count dataset above to more clearly show the 
# distribution of cut within colour, or colour within cut?

# original
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# improvement
diamonds %>% 
  count(color, cut) %>%
  count(color, wt = n)

# therefore
diamonds %>%
  count(color, cut) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(color, cut)) +
  geom_tile(aes(fill = prop))
# cut within color

diamonds %>%
  count(cut, color) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(aes(color, cut)) +
  geom_tile(aes(fill = prop))

# 2. Use geom_tile() together with dplyr to explore how average flight 
# delays vary by destination and month of year. What makes the plot 
# difficult to read? How could you improve it?
flights %>%
  group_by(dest, month) %>%
  mutate(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(factor(month), dest)) +
  geom_tile(aes(fill = avg_delay)) +
  labs(x = "Month", y = "Destination", fill = "Average Delay")
  

flights %>%
  group_by(month, dest) %>%
  mutate(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = factor(month), y = dest)) +
  geom_tile(mapping = aes(fill = avg_delay)) + 
  labs(x = "Month", y = "Destination", fill = "Average Delay")

# from jrnold's github:
# There are several things that could be done to improve it, sort
# destinations by a meaningful quanity (distance, number of flights, 
# average delay) remove missing values better color scheme (viridis)
# How to treat missing values is difficult. In this case, missing values 
# correspond to airports which don’t have regular flights (at least one 
# flight each month) from NYC. These are likely smaller airports 
# (with higher variance in their average due to fewer observations).

# 3 Why is it slightly better to use aes(x = color, y = cut) rather than 
# aes(x = cut, y = color) in the example above?

diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut)) + 
  geom_tile(mapping = aes( fill = n))

diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = cut, y = color)) + 
  geom_tile(mapping = aes( fill = n))

# easier to read longer labels in y axis
# from jrnold's github:
# Another justification, for switching the order is that the larger 
# numbers are at the top when x = color and y = cut, and that lowers 
# the cognitive burden of interpreting the plot.

# 7.5.3.1===================================================================
# 1. Instead of summarising the conditional distribution with a boxplot, you
# could use a frequency polygon. What do you need to consider when using 
# cut_width() vs cut_number()? How does that impact a visualisation of the 
# 2d distribution of carat and price?

ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.3)))

ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 10)))

ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(color = cut_width(carat, 0.3)))

ggplot(diamonds, aes(x = price)) +
  geom_freqpoly(aes(color = cut_number(carat, 10)))

# When using cut_width the number in each bin may be unequal. The 
# distribution of carat is right skewed so there are few diamonds in 
# those bins.

ggplot(diamonds, aes(price, ..density..)) +
  geom_freqpoly(aes(color = cut_number(carat, 10)))

ggplot(diamonds, aes(price, ..density..)) +
  geom_freqpoly(aes(color = cut_width(carat, 0.3)))

# 2. Visualise the distribution of carat, partitioned by price.

ggplot(diamonds) +
  geom_boxplot(aes(cut_width(price, 1000), carat)) +
  coord_flip()

ggplot(diamonds) +
  geom_lv(aes(cut_width(price, 1000), carat), fill = "gold") +
  coord_flip()

ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = cut_number(price, 10), y = carat)) +
  coord_flip()

ggplot(data = diamonds) +
  geom_lv(aes(cut_number(price, 10), carat), fill = "gold") +
  coord_flip()
# 3. How does the price distribution of very large diamonds compare to small
# diamonds. Is it as you expect, or does it surprise you?
ggplot(data = diamonds) +
  geom_lv(aes(cut_width(carat, 0.5), price), fill = "gold") +
  coord_flip() + 
  xlab("Carat") + ylab("Price")
# smaller carats have less variation in distribution of price.

# 4. Combine two of the techniques you’ve learned to visualise the combined 
# distribution of cut, carat, and price.
ggplot(data = diamonds) +
  geom_lv(aes(cut_width(carat, 0.5), price), fill = "gold") +
  facet_wrap(~ cut, ncol = 1)

# some others:
ggplot(data = diamonds,  aes(x = carat, y = price)) +
  geom_hex() +
  facet_wrap(~ cut, ncol = 1)

ggplot(diamonds, 
       aes(cut_number(carat, 5), y = price)) +
  geom_boxplot(aes(color = cut))

ggplot(diamonds, 
       aes(cut_number(carat, 5), price)) +
  geom_boxplot(aes(fill = cut))

ggplot(data = diamonds, 
       aes(cut_number(carat, 5), price)) +
  geom_lv(aes(color = cut, fill = cut))

ggplot(diamonds,
       aes(color = cut_number(carat, 5), 
           fill = cut_number(carat, 5),
           x = cut,
           y = price)) +
  geom_boxplot()

ggplot(diamonds,
       aes(color = cut_number(carat, 5),
           x = cut,
           y = price)) +
  geom_boxplot()

ggplot(data = diamonds,
       aes(color = cut_number(carat, 5), 
           fill = cut_number(carat, 5),
           x = cut,
           y = price)) +
  geom_lv()

ggplot(data = diamonds,
       aes(fill = cut_number(carat, 5),
           x = cut,
           y = price)) +
  geom_lv()

# 5. Two dimensional plots reveal outliers that are not visible in one 
# dimensional plots. For example, some points in the plot below have an 
# unusual combination of x and y values, which makes the points outliers 
# even though their x and y values appear normal when examined separately.

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

# Why is a scatterplot a better display than a binned plot for this case?

# Because each outlier is an outlier in the higher dimension but within
# normal range for x and for y values. Take the point around 6.7 for x and 
# 4 for y, an outlier for the data clearly seen with scatterplot BUT in 
# binned plot both the x value and y value are NOT outliers in terms of 
# just x or just y value