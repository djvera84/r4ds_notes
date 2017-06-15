# R for Data Sceince Chapter 4 Exercises
# Daniel J. Vera, Ph.D. 

library(nycflights13)
library(tidyverse)

# Pick observations by values:      filter()
# Reorder the rows (sort):          arrange()
# Pick variables by their names:    select()
# Create new varialbes from old:    mutate()
# Collapse many values to summary:  sumarize()

# group_by() changes scope of each function
# to operate on subgroups of data set.

# for these functions:
# function(data_frame, then what to do using variable names) 
# gives new data frame

# 5.2.4 on website http://r4ds.had.co.nz/transform.html

#1) Find all flights that

# Had an arrival delay of two or more hours
# Flew to Houston (IAH or HOU)
# Were operated by United, American, or Delta
# Departed in summer (July, August, and September)
# Arrived more than two hours late, but didn’t leave late
# Were delayed by at least an hour, but made up over 30 minutes 
#in flight
# Departed between midnight and 6am (inclusive)

filter(flights, arr_delay >= 120)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, carrier %in% c("UA", "AA", "DL"))
filter(flights, month %in% c(7, 8, 9))
filter(flights, arr_delay >= 120, dep_delay <= 0)
filter(flights, dep_delay >= 60, arr_delay - dep_delay >= 30)
filter(flights, dep_time >= 0 & dep_time <= 600)

?between
filter(flights, between(dep_time, 0, 600))

filter(flights, is.na(dep_time))

NA^0
NA | TRUE
NA & FALSE
NA*0
# in conditional expressions, NA is ignored.
# Any operation on NA is generally NA.
#__________

# 5.3.1 on website

arrange(flights, !is.na(arr_time))
arrange(flights, desc(arr_delay))
arrange(flights, dep_delay)
arrange(flights, desc(distance/air_time))
arrange(flights, desc(distance))
arrange(flights, distance)
#__________

# 5.4.1 on website

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, starts_with("dep"), starts_with("arr"))

select(flights, dep_time, dep_time)

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

select(flights, contains("TIME"))
# ignores case

select(flights, contains("TIME", ignore.case = FALSE))

# smaller data set for mutate study
flights_sml <- select(flights, year:day,
                      ends_with("delay"), 
                      distance,
                      air_time)
#__________

# 5.5.2 on website

transmute(flights, 
       dep_from_00 = (dep_time %/% 100)*60 + dep_time %% 100,
       sched_from_00 = (sched_dep_time %/% 100)*60 + 
                        sched_dep_time %% 100)

flights2 <- select(flights, air_time, arr_time, dep_time)
mutate(flights2, air_time_new = arr_time - dep_time)

select(flights, dep_time, sched_dep_time, dep_delay)
#Find the 10 most delayed flights using a ranking function.
#How do you want to handle ties? Carefully read the 
#documentation for min_rank()

flights_delayed <- flights%>%
  mutate(ranked_delay = min_rank(desc(arr_delay))) %>%
  arrange(ranked_delay)

#What does 1:3 + 1:10 return? Why?
1:3 + 1:10

#What trigonometric functions does R provide?
?Trig

#__________

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# 5.6.7 on website

# Brainstorm at least 5 different ways to assess the typical 
# delay characteristics of a group of flights. Consider the 
# following scenarios:

# A flight is 15 minutes early 50% of the time, 
# and 15 minutes late 50% of the time.

flights %>%
  group_by(flight) %>%
  summarize(
    early_15 = sum(arr_delay <= -15, na.rm = TRUE)/n(),
    late_15 = sum(arr_delay >= 15, na.rm = TRUE)/n()) %>%
  filter(early_15 == 0.5, late_15 == 0.5)

# A flight is always 10 minutes late.
flights %>%
  group_by(flight) %>%
  summarize(
    late_10 = sum(arr_delay == 10, na.rm = TRUE)/n()
  ) %>%
  filter(late_10 == 1)

# A flight is 30 minutes early 50% of the time, 
# and 30 minutes late 50% of the time.
flights %>%
  group_by(flight) %>%
  summarize(
    early_30 = sum(arr_delay <= -30, na.rm = TRUE)/n(),
    late_30 = sum(arr_delay >= 30, na.rm = TRUE)/n()
    ) %>%
  filter(early_30 == 0.5, late_30 == 0.5)
# 99% of the time a flight is on time. 1% of the time 
# it’s 2 hours late.
flights %>%
  group_by(flight) %>%
  summarize(
    on_time = sum(arr_delay == 0, na.rm = TRUE)/n(),
    late_2hrs = sum(arr_delay >= 120, na.rm = TRUE)/n()
  ) %>%
  filter(on_time == 0.99, late_2hrs == 0.01)
# Which is more important: arrival delay or departure delay?
#
# Come up with another approach that will give you the same
# output as 

not_cancelled %>% count(dest)
not_cancelled %>% count(tailnum, wt = distance) 
#(without using count()).
not_cancelled %>%
  group_by(dest) %>%
  summarize(n = n())

not_cancelled %>%
  group_by(tailnum) %>%
  summarize(n = sum(distance))

# Our definition of cancelled flights 
# (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. 
# Why? Which is the most important column?
# It's redundant. Basically a flight that never arrived
# also never departed so can just use dep_delay variable

# Look at the number of cancelled flights per day. 
# Is there a pattern? 
# Is the proportion of cancelled flights related to the average 
# delay?
flights %>%
  group_by(day) %>%
  summarize(
    cancelled = sum(is.na(dep_delay)),
    average_delay = mean(dep_delay,na.rm = TRUE)
  ) %>%
  ggplot(aes(x=average_delay, y=cancelled)) +
  geom_point() +
  geom_smooth(se = FALSE)

flights %>%
  group_by(day) %>%
  summarize(
    prop_cancelled = sum(is.na(dep_delay))/n(),
    average_delay = mean(dep_delay,na.rm = TRUE)
  ) %>%
  ggplot(aes(x=average_delay, y=prop_cancelled)) +
  geom_point() +
  geom_smooth(se = FALSE) 

flights %>%
  filter(is.na(dep_delay)) %>%
  count(day)
  
# Which carrier has the worst delays? 
# Challenge: can you disentangle the effects of 
# bad airports vs. bad carriers? Why/why not? 
# Hint: think about 
flights %>% 
  group_by(carrier, dest) %>% 
  summarize(n())            

flights %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_delay))

flights %>%
  group_by(carrier, dest) %>%
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
  group_by(carrier) %>%
  summarize(mean_delay_mad = mad(mean_delay, na.rm = TRUE)) %>%
  arrange(desc(mean_delay_mad))

# For each plane, count the number of flights before the first 
# delay of greater than 1 hour.
temp <- 
  flights %>%
  group_by(tailnum) %>% 
  mutate(row = row_number()) %>%
  filter(arr_delay > 60) %>%
  summarize(flights_before_1hr_delay = first(row) - 1)

?count
# Sort argument sorts the results of count in descending order.

#__________
# 5.7.1 on website

# Refer back to the table of useful mutate and filtering functions. 
# Describe how each operation changes when you combine it with grouping.
# It performs by group instead of over entire data frame.

# Which plane(tailnum) has the worst on-time record?
# Define "on-time" as arr_delay <= 30min
flights %>%
  group_by(tailnum) %>%
  summarize(prop_on_time = sum(arr_delay <= 30, na.rm = TRUE)/n(),
            mean_arr_delay = mean(arr_delay, na.rm = TRUE),
            flights = n()) %>%
  arrange(prop_on_time, desc(mean_arr_delay))

# What time of day should you fly if you want to avoid delays 
# as much as possible?
flights %>%
  group_by(hour) %>%
  summarize(prop_delay = sum(arr_delay > 0, na.rm = TRUE)/n()) %>%
  ggplot(mapping = aes(x = hour, y = prop_delay)) +
  geom_col()

# For each destination, compute the total minutes of delay. 
# For each, flight, compute the proportion of the total delay 
# for its destination.

flights %>%
  group_by(dest) %>%
  mutate(
    total_delay = sum(arr_delay > 0, na.rm = TRUE) + 
                  sum(dep_delay > 0, na.rm = TRUE),
    prop_delay = total_delay/n()
  )

# Delays are typically temporally correlated: even once the problem 
# that caused the initial delay has been resolved, later flights are
# delayed to allow earlier flights to leave. Using lag() explore how 
# the delay of a flight is related to the delay of the immediately 
# preceding flight.

flights %>%
  group_by(year, month, day) %>%
  filter(!is.na(dep_delay)) %>%
  mutate(previous_dep_delay = lag(dep_delay)) %>%
  ggplot(mapping = aes(x = previous_dep_delay, y = dep_delay)) +
  geom_point() + 
  geom_smooth()

# Look at each destination. Can you find flights that are suspiciously 
# fast? (i.e. flights that represent a potential data entry error). 
# Compute the air time a flight relative to the shortest flight to that 
# destination. Which flights were most delayed in the air?

flights %>%
  filter(!is.na(air_time)) %>% # I originally did not put this but
  # its a good idea, found solution on web
  group_by(dest) %>%
  mutate(mean_time_to_dest = mean(air_time, na.rm = TRUE),
         fast = air_time/mean_time_to_dest - 1) %>%
  arrange(desc(fast)) %>%
  select(flight, dest, mean_time_to_dest, fast, everything())
# above is my solution for first part, second part follows:
flights %>%
  filter(!is.na(air_time)) %>% #see above comment
  group_by(flight) %>%
  mutate(shortest_time = min(air_time, na.rm = TRUE),
         flight_to_shortest = air_time - shortest_time) %>%
  ggplot(mapping = aes(x = shortest_time, y = flight_to_shortest)) +
  geom_point() + 
  geom_smooth()

# The following solution comes from
# https://jrnold.github.io/e4qf/

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(med_time = median(air_time),
         fast = (air_time - med_time) / med_time) %>%
  arrange(fast) %>%
  select(air_time, med_time, fast, dep_time, sched_dep_time, arr_time, sched_arr_time) %>%
  head(15)
# Basically the same as above but uses median. He also has one
# with a Z-score
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(air_time_mean = mean(air_time),
         air_time_sd = sd(air_time),
         z_score = (air_time - air_time_mean) / air_time_sd) %>%
  arrange(z_score) %>%
  select(z_score, air_time_mean, air_time_sd, air_time, dep_time, sched_dep_time, arr_time, sched_arr_time)

# To answer the second question, he used difference instead of
# ratio like me and arranged instead of graphed. I think the
# difference is in fact better since shorter flight times
# will make the ratio larger so I changed mine. The book is good
# but exercises are not always worded the best they could be.
# E.g. 'relative' in my head means ratio but after looking at data
# difference (minus) is probably better.
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(air_time_diff = air_time - min(air_time)) %>%
  arrange(desc(air_time_diff)) %>%
  select(dest, year, month, day, carrier, flight, air_time_diff, air_time, dep_time, arr_time) %>%
  head()

# If I graph his I get
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  mutate(min_air_time = min(air_time),
        air_time_diff = air_time - min(air_time)) %>%
  ggplot(mapping = aes(x = min_air_time, y = air_time_diff)) +
  geom_point()

# Find all destinations that are flown by at least two carriers. 
# Use that information to rank the carriers.
flights %>%
  group_by(dest) %>%
  count(carrier) %>% #counting flights or rows in overall data
  group_by(carrier) %>% #now grouped so next line 
  count(sort = TRUE)    #will count carriers and sort
 
