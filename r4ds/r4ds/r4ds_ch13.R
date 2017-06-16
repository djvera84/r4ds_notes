# R for Data Science Chapter 13
# Dates and Times with lubridate
# Daniel J. Vera, Ph.D.
library(tidyverse)
library(lubridate)
library(nycflights13)

# Creating Date/Times ======================================================

# A date, <date>
# A time, <time>
# A date-time, typically to nearest second <dttm>, aka POSIXct.

today() # <date>
now()   # <dttm>

# From Strings
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

# Also use unquoted numbers:
ymd(20170131)

ymd_hms("2017-01 31 20:11:59")
mdy_hm("01/31/2017 08:01")

ymd(20170131, tz = "UTC") # <dttm>

# From Individual Components
flights %>%
  select(year, month, day, hour, minute)

flights %>%
  select(year, month, day, hour, minute) %>%
  mutate(
    departure = make_datetime(year, month, day, hour, minute)
  )

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt
View(flights_dt)

flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600) # 600 seconds = 10 minutes

# From Other Types

as_datetime(today())
as_date(now())
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

# Exercises 16.2.4 on website:
# http://r4ds.had.co.nz/dates-and-times.html#exercises-45
# 1. What happens if you parse a string that contains invalid dates?

ymd(c("2010-10-10", "bananas"))

# you get an NA for the string and a warning message.

# 2. What does the tzone argument to today() do? Why is it important?
# tzone is a character vector specifiying which time zoon you would like,
# default is set to the timezone set on computer. Since different timezones
# can have different dates (International date line), the value will be
# different.
today(tzone = "GMT")
today(tzone = "NZ")
today(tzone = "GMT") == today(tzone = "NZ")
# 3. Use the appropriate lubridate function to parse each of the following
# dates:
  
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

# Date-Time Components =====================================================
# Getting Components
datetime <- ymd_hms("2016-07-08 12:3")

year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

# More flights depart during the week than on the weekend:
flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  ggplot(aes(x = wday)) +
    geom_bar()

# interesting anomaly/pattern
flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
    geom_line()
  
sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

# Rounding

# example: plot flights per week
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) + 
    geom_line()

# Setting Components
(datetime <- ymd_hms("2016-07-08 12:34:56"))

year(datetime) <- 2020
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

ymd("2015-02-01") %>%
  update(mday = 30)

ymd("2015-02-01") %>%
  update(hour = 400)

# distribution of flights across the course of the day
# for every day of the year:
flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  ggplot(aes(dep_hour)) +
    geom_freqpoly(binwidth = 300)

# Exercises 16.3.4 on website:
# http://r4ds.had.co.nz/dates-and-times.html#exercises-46
# 1. How does the distribution of flight times within a day change over the
# course of the year?
flights_dt %>%
  mutate(
    time = hour(dep_time) * 100 + minute(dep_time),
    mon = as.factor(month(dep_time))
  ) %>%
  ggplot(aes(x = time, y = ..density.., group = mon, color = mon)) +
    geom_freqpoly(binwidth = 100) 
    # binwidth = 100, since 100 units on x axis corresponds to 1 hour

# 2. Compare dep_time, sched_dep_time and dep_delay. Are they consistent? 
# Explain your findings.
flights_dt %>%
  mutate(dep_time_check = sched_dep_time + dep_delay * 60) %>% 
         # sub gives seconds
  filter(dep_time != dep_time_check) %>%
  select(dep_time_check, dep_time, everything())

# From jrnold's solutions:
# There exist discrepencies. It looks like there are mistakes in the dates.
# These are flights in which the actual departure time is on the next day 
# relative to the scheduled departure time. We forgot to account for this 
# when creating the date-times. The code would have had to check if the 
# departure time is less than the scheduled departure time. Alternatively, 
# simply adding the delay time is more robust because it will automatically
# account for crossing into the next day.

# 3. Compare air_time with the duration between the departure and arrival. 
# Explain your findings. (Hint: consider the location of the airport.)
flights_dt %>%
  mutate(flight_duration = as.numeric(arr_time - dep_time),
         air_time_mins = air_time,
         diff = flight_duration - air_time_mins) %>%
  select(origin, dest, flight_duration, air_time_mins, diff)

# 4. How does the average delay time change over the course of a day? 
# Should you use dep_time or sched_dep_time? Why?
# From jrnold:
# Use sched_dep_time because that is the relevant metric for someone 
# scheduling a flight. Also, using dep_time will always bias delays 
# to later in the day since delays will push flights later. This 
# makes sense since we are trying to measure DELAY which is measured
# relative to the scheduled time, not actual.
flights_dt %>%
  mutate(flight_hour = hour(sched_dep_time)) %>%
  group_by(flight_hour) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(flight_hour, avg_delay)) +
    geom_point() +
    geom_smooth()

# 5. On what day of the week should you leave if you want to minimize 
# the chance of a delay?
flights_dt %>%
  mutate(wday_of_flight = wday(sched_dep_time)) %>%
  group_by(wday_of_flight) %>%
  summarize(avg_delay_dep = mean(dep_delay),
            avg_delay_arr = mean(arr_delay, na.rm = TRUE))

# Sunday

# Also some visuals:
library(lvplot)
flights_dt %>%
  mutate(wday_of_flight = wday(sched_dep_time)) %>%
  group_by(wday_of_flight) %>%
  ggplot(aes(as.factor(wday_of_flight), y = dep_delay)) +
    geom_lv()

flights_dt %>%
  mutate(wday_of_flight = wday(sched_dep_time)) %>%
  group_by(wday_of_flight) %>%
  ggplot(aes(as.factor(wday_of_flight), y = arr_delay)) +
  geom_lv()

# 6. What makes the distribution of diamonds$carat and 
# flights$sched_dep_time similar?

ggplot(diamonds, aes(diamonds$carat)) +
  geom_bar()
ggplot(flights, aes(flights$sched_dep_time)) +
  geom_bar()


ggplot(diamonds, aes(x = carat %% 1 * 100)) +
  geom_histogram(binwidth = 1)
ggplot(flights_dt, aes(x = minute(sched_dep_time))) +
  geom_histogram(binwidth = 1)

# both have nice 'human' numbers. for flights its 00 min and 30 min,
# with minutes ending in either 0 or 5
# for diamonds its carats at 0, 1/3, 1/2, 2/3.

# 7. Confirm my hypothesis that the early departures of flights in 
# minutes 20-30 and 50-60 are caused by scheduled flights that leave early.
# Hint: create a binary variable that tells you whether or not a flight was
# delayed.
# minute level
flights_dt %>%
  mutate(early = dep_delay < 0,
         minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarize(early = mean(early)) %>%
  ggplot(aes(minute, early)) +
    geom_point()

# 10 minute level
flights_dt %>%
  mutate(early = dep_delay < 0,
         minute = minute(sched_dep_time) %% 10) %>%
  group_by(minute) %>%
  summarise(early = mean(early)) %>%
  ggplot(aes(minute, early)) +
    geom_point()

# Time Spans ===============================================================
# Durations represent exact number of seconds
# Periods represent human units, weeks, months
# Intervals represent starting and ending point

# Durations
h_age <- today() - ymd(19791014) # difftime class
h_age
d_age <- today() - ymd(19840920)
d_age

as.duration(h_age)
as.duration(d_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)

tommorw <- today() + ddays(1)
last_year <- today() - dyears(1)

one_pm <- ymd_hms(
  "2016-03-12 13:00:00",
  tz = "America/New_York"
)
one_pm
one_pm + ddays(1)

# Periods
one_pm
one_pm + days(1)

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# daylight savings
one_pm + ddays(1)
one_pm + days(1)

flights_dt %>%
  filter(arr_time < dep_time)

flights_dt <- flights_dt %>%
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )

flights_dt %>%
  filter(overnight, arr_time < dep_time)

# Intervals

years(1) / days(1)

next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

# Exercises 16.4.5 on website:
# http://r4ds.had.co.nz/dates-and-times.html#exercises-47
# 1. Why is there months() but no dmonths()?
# Months do not contain a fixed number of days. They can have
# 28 - 31 days. How do you determine seconds?

# 2. Explain days(overnight * 1) to someone who has just started learning R.
# How does it work?
# overnight evaluates to TRUE or FALSE and TRUE corresponds to 1
# while FALSE corresponds to 0. Thus TRUE * 1 == 1 and
# FALSE * 1 == 0.

# 3. Create a vector of dates giving the first day of every month in 2015. 
# Create a vector of dates giving the first day of every month in the 
# current year.
ymd("2015-01-01") + months(0:11)
ymd("2017-01-01") + months(0:11)

# 4. Write a function that given your birthday (as a date), returns how 
# old you are in years.
how_old <- function(bday) {
  (bday %--% today()) %/% years(1)
}

# 5. Why can’t (today() %--% (today() + years(1)) / months(1) work?
# (today() %--% (today() + years(1)) / months(1)
# Not sure.

# Time Zones ===============================================================
Sys.timezone()
OlsonNames()
length(OlsonNames())
head(OlsonNames())

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York")) 
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1 - x2
x1 - x3

x4 <- c(x1, x2, x3)
x4

# keep instant in time, change display
x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a

x4a - x4

# change instant
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b

x4b - x4
