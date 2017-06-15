# R for Data Science Chapter 5
# Exploratory Data Analysis
# Daniel J. Vera, Ph.D.

library(tidyverse)
library(nycflights13)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%
  count(cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>%
  count(cut_width(carat, 0.5))

smaller_diamonds <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller_diamonds, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller_diamonds, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = smaller_diamonds, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwdith = 0.25)

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>%
  # trying to pick up on unusual values in previous graph
  filter(y < 3 | y > 20) %>% 
  arrange(y) 

# Exercises 7.3.4 on website
#===============================================================
# Explore the distribution of each of the x, y, and z variables 
# in diamonds. What do you learn? Think about a diamond and how 
# you might decide which dimension is the length, width, and depth.
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = x), binwidth = 0.5)

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = z), binwidth = 0.5)

# Explore the distribution of price. Do you discover anything unusual 
# or surprising? 
# (Hint: Carefully think about the binwidth and make sure you try a 
# wide range of values.)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram()

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram(binwidth = 100)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram(binwidth = 100) +
  coord_cartesian(xlim = c(0, 5000))

diamonds %>% 
  filter(price < 1500) %>%
  count()

diamonds %>% 
  filter(price > 1500) %>%
  count()

# How many diamonds are 0.99 carat? How many are 1 carat? 
# What do you think is the cause of the difference?

ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram(binwidth = .01) +
  coord_cartesian(xlim = c(0.90, 1.1))

diamonds %>% 
  filter(carat == 1) %>%
  count()

diamonds %>% 
  filter(carat == 0.99) %>%
  count()
# most likely the 0.99 are ones made with the intention of being
# 1 carat and there is some error that makes it 0.99 or there is 
# a measurement error another reason from online solutions:
# Around 1500 diamonds are 1.001.00 carat, compared to around 30 
# or so diamonds at .99.99 carat. This could occur because prospective 
# buyers of diamonds, if they are already willing to buy a .99.99 carat 
# diamond will decide it is more aesthetically pleasing to say they 
# bought a 11 carat diamond so there is less demand for .99.99 carat 
# diamonds.

# Compare and contrast coord_cartesian() vs xlim() or ylim() when 
# zooming in on a histogram. What happens if you leave binwidth unset? 
# What happens if you try and zoom so only half a bar shows?
ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram(binwidth = .01) +
  coord_cartesian(xlim = c(0.90, 1.1))

ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram(binwidth = .01) +
  xlim(0.90, 1.1)

ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0.90, 1.1))

ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram() +
  xlim(0.90, 1.1)

#===============================================================
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

# Exercises 7.4.1 on website
#===============================================================
# What happens to missing values in a histogram? What happens to 
# missing values in a bar chart? Why is there a difference?
ggplot(data = flights, mapping = aes(x = dep_delay)) +
  geom_histogram()

flights %>%
  mutate(carrier = ifelse(carrier == "AA", NA, carrier)) %>%
  ggplot(aes(carrier)) +
  geom_bar()

# Histograms omit missing values, bar charts draw them as another
# category

# What does na.rm = TRUE do in mean() and sum()?
# It removes the NA missing values before computing.
#===============================================================

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut)) +
  geom_bar()

ggplot(
  data = diamonds,
  mapping = aes(x = price, y = ..density..)
  ) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
                
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg) + 
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median), 
      y = hwy
      )
    ) +
  coord_flip()
      
# Exercises 7.5.1.1 on website
#===============================================================
# Use what you’ve learned to improve the visualisation of the 
# departure times of cancelled vs. non-cancelled flights.

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time, y = ..density..)) + 
  geom_freqpoly(
    mapping = aes(colour = cancelled), 
    size = 1.5, 
    binwidth = 1/4)

# What variable in the diamonds dataset is most important for predicting the   
# price of a diamond? How is that variable correlated with cut? Why does the 
# combination of those two relationships lead to lower quality diamonds 
# being more expensive?

diamonds_numeric <- diamonds[-c(2, 3, 4)]
cor(diamonds_numeric)

lm_diamonds = lm(price ~ ., data = diamonds)
summary(lm_diamonds)

lm_diamonds_carat = lm(price ~ carat, data = diamonds)
summary(lm_diamonds_carat)

# carat looks like the most highly correlated.
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth()

ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) + 
  geom_boxplot()

# it looks like on average, ideal cuts have lower carat and since
# carat is variable with highest correlation to price, ideal cuts
# will be cheaper, on average.

# Install the ggstance package, and create a horizontal boxplot. 
# How does this compare to using coord_flip()?
ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) + 
  geom_boxplot() +
  coord_flip()

# install.packages("ggstance") 
library(ggstance) # should go in beginning but see next comment
#bad form generally in scripts to install packages but for HW
ggplot(data = diamonds, mapping = aes(x = carat, y = cut)) + 
  geom_boxploth()

# To create a horizontal layer in ggplot2 with coord_flip(), you 
# have to supply aesthetics as if they were to be drawn vertically
# then flip with coord_flip()
# In ggstance you do it as if drawn horizontally, see above.

# One problem with boxplots is that they were developed in an era of 
# much smaller datasets and tend to display a prohibitively large number 
# of “outlying values”. One approach to remedy this problem is the 
# letter value plot. 
# Install the lvplot package, and try using geom_lv() to display the 
# distribution of price vs cut. What do you learn? How do you interpret 
# the plots?

#install.packages("lvplot") # see above comment on installing packages
library(lvplot) # should at begining but see above

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + 
  geom_lv()

# Compare and contrast geom_violin() with a facetted geom_histogram(), 
# or a coloured geom_freqpoly().
# What are the pros and cons of each method?

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price)) + 
  facet_grid(cut ~ .)

ggplot(data = diamonds, 
       mapping = aes(x = price, y = ..density.., color = cut)) +
  geom_freqpoly()

ggplot(data = diamonds) +
  geom_violin(mapping = aes(x = cut, y = price)) + 
  coord_flip()

# If you have a small dataset, it’s sometimes useful to use geom_jitter() 
# to see the relationship between a continuous and categorical variable. 
# The ggbeeswarm package provides a number of methods similar to 
# geom_jitter(). List them and briefly describe what each one does.
#install.packages("ggbeeswarm")
library(ggbeeswarm)
?ggbeeswarm

ggplot(data = mpg) +
  geom_quasirandom(mapping = aes(x = reorder(class, hwy, FUN = median),
                                 y = hwy))

ggplot(data = mpg) +
  geom_beeswarm(mapping = aes(x = reorder(class, hwy, FUN = median),
                              y = hwy))


# The beeswarm geom is a convenient means to offset points within categories 
# to reduce overplotting. Uses the beeswarm package

# The quasirandom geom is a convenient means to offset points within 
# categories to reduce overplotting. Uses the vipor package.

# Both similar to geom_jitter()
#===============================================================
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# Exercises 7.5.2.1 on website
#===============================================================
# How could you rescale the count dataset above to more clearly show 
# the distribution of cut within color, or color within cut?

# calculate proportion
diamonds %>%
  count(cut, color) %>%
  group_by(color) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))

diamonds %>%
  count(cut, color) %>%
  group_by(cut) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))


# Use geom_tile() together with dplyr to explore how average flight 
# delays vary by destination and month of year. 
# What makes the plot difficult to read? How could you improve it?

flights %>%
  group_by(month, dest) %>%
  mutate(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = factor(month), y = dest)) +
  geom_tile(mapping = aes(fill = avg_delay)) + 
  labs(x = "Month", y = "Destination", fill = "Average Delay")

# Why is it slightly better to use aes(x = color, y = cut) rather than 
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
# from solutions on web:
# Another justification, for switching the order is that the larger 
# numbers are at the top when x = color and y = cut, and that lowers 
# the cognitive burden of interpreting the plot.
#===============================================================
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_point(
    mapping = aes(x = carat, y = price),
    alpha = 1/100
    )

ggplot(data = smaller_diamonds) + 
  geom_bin2d(mapping = aes(x = carat, y = price))

#install.packages("hexbin")
library(hexbin)
ggplot(data = smaller_diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller_diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller_diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20))) 

# Exercises 7.5.3.1 on website
#===============================================================
# Instead of summarising the conditional distribution with a boxplot, 
# you could use a frequency polygon. What do you need to consider 
# when using cut_width() vs cut_number()? How does that impact a 
# visualisation of the 2d distribution of carat and price?
ggplot(data = diamonds) +
  geom_freqpoly(
    mapping = aes(x = price, color = cut_width(carat, 0.3))
  )

ggplot(data = diamonds) +
  geom_freqpoly(
    mapping = aes(x = price, 
                  y = ..density..,
                  color = cut_width(carat, 0.3))
  )

ggplot(data = diamonds) +
  geom_freqpoly(
    mapping = aes(x = price, color = cut_number(carat, 10))
  )

ggplot(data = diamonds) +
  geom_freqpoly(
    mapping = aes(x = price, 
                  y = ..density..,
                  color = cut_number(carat, 10))
  )

# Visualise the distribution of carat, partitioned by price.
ggplot(data = diamonds, mapping = aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

ggplot(diamonds, aes(x = cut_width(price, 2000, boundary = 0), y = carat)) +
  geom_boxplot(varwidth = TRUE) +
  coord_flip() +
  xlab("Price")

# How does the price distribution of very large diamonds compare to 
# small diamonds. Is it as you expect, or does it surprise you?

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, .3))) 

# More variance in large diamonds. 
 
# Combine two of the techniques you’ve learned to visualise the combined 
# distribution of cut, carat, and price.

# Instead of summarizing the conditional distribution with a boxplot, 
# you could use a frequency polygon. What do you need to consider when 
# using cut_width() versus cut_number()? How does that impact a 
# visualization of the 2D distribution of carat and price? 


ggplot(data = diamonds, 
       mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.3)))

ggplot(data = diamonds, 
       mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 10)))

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut_width(carat, 0.3)))

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut_number(carat, 10)))

# You need to pay attention to the width of carat you are looking at
# or the number of bins. 

# Visualize the distribution of carat, partitioned by price. 
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = cut_number(price, 10), y = carat)) +
  coord_flip()

ggplot(data = diamonds) +
  geom_lv(mapping = aes(x = cut_number(price, 10), y = carat)) +
  coord_flip()

# How does the price distribution of very large diamonds compare to 
# small diamonds. Is it as you expect, or does it surprise you?
ggplot(data = diamonds) +
  geom_lv(mapping = aes(x = cut_width(carat, 0.5), y = price)) +
  coord_flip() +
  xlab('Carat')

ggplot(data = diamonds) +
  geom_lv(mapping = aes(x = cut_number(carat, 5), y = price)) +
  coord_flip() +
  xlab('Carat')

# smaller carats have less variation in distribution of price.


# Combine two of the techniques you have learned to visualize the 
# combined distribution of cut, carat, and price. 
ggplot(data = diamonds,  mapping = aes(x = carat, y = price)) +
  geom_hex() +
  facet_wrap(~ cut, ncol = 1)

ggplot(data = diamonds, 
       mapping = aes(x = cut_number(carat, 5), y = price)) +
  geom_boxplot(mapping = aes(color = cut))

ggplot(data = diamonds, 
       mapping = aes(x = cut_number(carat, 5), y = price)) +
  geom_lv(mapping = aes(color = cut, fill = cut))

ggplot(data = diamonds,
       aes(color = cut_number(carat, 5), 
           fill = cut_number(carat, 5),
           x = cut,
           y = price)) +
  geom_boxplot()

ggplot(data = diamonds,
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

# Two-dimensional plots reveal outliers that are not visible in 
# one-dimensional plots. For example, some points in the following plot 
# have an unusual combination of x and y values, which makes the points
# outliers even though their x and y values appear normal when examined 
# separately: 

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = x, y = y)) + 
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11)) 

# Why is a scatterplot a better display than a binned plot for this case?
ggplot(data = diamonds, 
       mapping = aes(x = cut_width(x, 2), y = y)) +
  geom_lv() 

# take the point around 6.7 for x and 4 for y, an outlier for the data
# clearly seen with scatterplot BUT in binned plot both the x value and
# y value are NOT outliers in terms of just x or just y value
#===============================================================
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

 
ggplot(data = faithful) + 
  geom_lv(mapping = aes(x = cut_number(eruptions, 4), y = waiting))

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds3 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

ggplot(data = diamonds3) +
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds3) +
  geom_boxplot(mapping = aes(x = cut, y = resid))
