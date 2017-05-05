# R for Data Science Chapter 10
# Relational Data with dplyr
# Daniel J. Vera, Ph.D.
library(tidyverse)
library(nycflights13)

airlines
airports
planes
weather

# Exercises 13.2.1 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-26

# 1. Imagine you wanted to draw (approximately) the route each plane flies from 
# its origin to its destination. What variables would you need? What tables 
# would you need to combine?

# you need origin, dest, from flights.
# you want to draw the route so latitude (lat) and lognitude (lon) from 
# airports table.
# we need to merge flights with airports twice according to jrnlod's github.
# once to get location of origin and once to get location of dest.

# 2. I forgot to draw the relationship between weather and airports. What is 
# the relationship and how should it appear in the diagram?

# airports variable faa and weather variable origin

# 3. weather only contains information for the origin (NYC) airports. If it 
# contained weather records for all airports in the USA, what additional 
# relation would it define with flights?

# dest

# We know that some days of the year are “special”, and fewer people than 
# usual fly on them. How might you represent that data as a data frame? 
# What would be the primary keys of that table? How would it connect to 
# the existing tables?

# special dates table using date.

# Keys======================================================================
planes %>%
  count(tailnum) %>%
  filter(n > 1) #if each tailnum is unique, there should NOT be more than 1.
  
weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)

# Finding an explicit primary key is difficult! E.g.
flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1)

flights %>%
  count(year, month, day, tailnum) %>%
  filter(n > 1)

# Exercises 13.3.1 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-27
# 1. Add a surrogate key to flights.
flights %>%
  mutate(surrogate_key = row_number())

# 2. Identify the keys in the following datasets

Lahman::Batting
babynames::babynames
nasaweather::atmos
fueleconomy::vehicles
ggplot2::diamonds

# (You might need to install some packages and read some documentation.)

Lahman::Batting %>%
  count(playerID) %>%
  filter(n > 1)
# not just playerID

Lahman::Batting %>%
  count(playerID, yearID) %>%
  filter(n > 1)

# not just playerID and yearID
Lahman::Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1)
# primary keys are playerID, yearID, stint.

babynames::babynames %>%
  count(year, sex, name) %>%
  filter(n > 1)

# primary key is year, sex, name

nasaweather::atmos %>%
  count(lat, long, year, month)

# the key is lat, long, year, month.

fueleconomy::vehicles %>%
  count(id) %>%
  filter(n > 1)
# the key is ID

diamonds %>%
  count(carat, cut, color, clarity, depth, table, price, x, y, z) %>%
  filter(n > 1)
# there is no primary key. we can create a surrogate
diamonds %>% 
  mutate(id = row_number()) %>%
  count(id) %>%
  filter(n > 1)

# 3. Draw a diagram illustrating the connections between the Batting, Master, 
# and Salaries tables in the Lahman package. Draw another diagram that shows 
# the relationship between Master, Managers, AwardsManagers.
# How would you characterise the relationship between the Batting, Pitching, 
# and Fielding tables?

###

# Mutating Joins ===========================================================
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")

flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

# inner join
x <- tribble(
  ~key, ~val_x, 
     1, "x1", 
     2, "x2", 
     3, "x3" 
  ) 
y <- tribble(
  ~key, ~val_y, 
     1, "y1", 
     2, "y2", 
     4, "y3" 
  )

x %>%
  inner_join(y, by = "key")

# outer join

# one table with duplicate key
x <- tribble(
  ~key, ~val_x, 
     1, "x1", 
     2, "x2", 
     2, "x3", 
     1, "x4" 
  )
y <- tribble(
  ~key, ~val_y,
     1, "y1", 
     2, "y2"
  )

left_join(x, y, by = "key")

# two tables with duplicate keys, may be error, you get Cartesian product, 
# i.e. all possible combinations.

x <- tribble(
  ~key, ~val_x, 
  1, "x1", 
  2, "x2", 
  2, "x3", 
  3, "x4" 
)
y <- tribble(
  ~key, ~val_y,
  1, "y1", 
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

# defining key columns
flights2 %>%
  left_join(weather)

flights2 %>%
  left_join(planes, by = "tailnum")

flights2 %>%
  left_join(airports, c("dest" = "faa"))

flights2 %>%
  left_join(airports, c("origin" = "faa"))

# Exercises 13.4.6 on website:
# http://r4ds.had.co.nz/tidy-data.html#exercises-28
# 1. Compute the average delay by destination, then join on the airports 
# data frame so you can show the spatial distribution of delays. Here’s an
# easy way to draw a map of the United States:
  airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
# (Don’t worry if you don’t understand what semi_join() does — you’ll learn
# about it next.)

# You might want to use the size or colour of the points to display the 
# average delay for each airport.

flights_avg <- flights %>% 
  group_by(dest) %>%
  mutate(avg_delay_dest = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))

# by size and color
ggplot(flights_avg, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(size = avg_delay_dest, color = avg_delay_dest)) +
  coord_quickmap()

# by size
ggplot(flights_avg, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(size = avg_delay_dest)) +
  coord_quickmap()

# by color
ggplot(flights_avg, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(color = avg_delay_dest)) +
  coord_quickmap()
               
# 2. Add the location of the origin and destination (i.e. the lat and lon)
# to flights.

# 3. Is there a relationship between the age of a plane and its delays?

# 4. What weather conditions make it more likely to see a delay?

# 5. What happened on June 13 2013? Display the spatial pattern of delays, 
# and then use Google to cross-reference with the weather.