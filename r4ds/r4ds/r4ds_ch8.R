# R for Data Science Chapter 8
# Data Import with readr
# Daniel J. Vera, Ph.D.
library(tidyverse)

heights <- read_csv(
  "https://raw.githubusercontent.com/hadley/r4ds/master/data/heights.csv"
  )

read_csv("a, b, c 
         1, 2, 3
         4, 5, 6")

read_csv("The first line of metadata 
         The second line of metadata 
         x, y, z
         1, 2, 3", skip = 2)

read_csv("# A comment I want to skip 
         x, y, z
         1, 2, 3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE)

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

read_csv("a, b, c\n1,2,.", na = ".")

# Exercises 11.2.2 on website: http://r4ds.had.co.nz/data-import.html

# 1. What function would you use to read a file where fields were separated 
# with "|"?

# read_delim(); you put read_delim(file, delim = "|")

# 2. Apart from file, skip, and comment, what other arguments do read_csv() 
# and read_tsv() have in common?

# col_names, col_types, locale, na, quoted_na, quote, comment, trim_ws,
# n_max, guess_max, progress...pretty much all of them.

# 3. What are the most important arguments to read_fwf()?
# file, col_positions

# 4. Sometimes strings in a CSV file contain commas. 
# To prevent them from causing problems they need to be surrounded by a 
# quoting character, like " or '. By convention, read_csv() assumes that 
# the quoting character will be ", and if you want to change it you’ll need 
# to use read_delim() instead. What arguments do you need to specify to read 
# the following text into a data frame?

read_delim("x,y\n1,'a,b'", delim = ",", quote = "'")

# 5. Identify what is wrong with each of the following inline CSV files. 
# What happens when you run the code?

read_csv("a,b\n1,2,3\n4,5,6")
# missing third column variable name, can either add a comma or 'c':
read_csv("a,b,\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2,3\n4,5,6")

read_csv("a,b,c\n1,2\n1,2,3,4")
# missing forth column variable name, can either add a comma or 'd':
read_csv("a,b,c,\n1,2\n1,2,3,4")
read_csv("a,b,c,d\n1,2\n1,2,3,4")

read_csv("a,b\n\"1")
# closing quote.
read_csv("a,b\n1")

read_csv("a,b\n1,2\na,b") #looks ok?

read_csv("a;b\n1;3") 
#looks like should use read_delim() or read_csv2():
read_delim("a;b\n1;3", delim = ";")
read_delim("a;b\n1;3", ";")
read_csv2("a;b\n1;3")


#===========================================================================
str(parse_logical(c("TRUE","FALSE","NA")))
str(parse_integer(c("1", "2","3")))
str(parse_date(c("2010-01-01", "1979-10-14")))
parse_integer(c("1", "231", ".", "456"), na = ".")

x <- parse_integer(c("123","345","abc", "123.45"))
x
problems(x)

#===========================================================================
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))
parse_number("$100")
parse_number("20%")
parse_number("It cost $123.45")

# Used in America
parse_number("$123,456,789")

# Used in many parts of Europe
parse_number(
  "123.456.789",
  locale = locale(grouping_mark = ".")
)

# Used in Switzerland
parse_number(
  "123'456'789",
  locale = locale(grouping_mark = "'")
)
#===========================================================================

charToRaw("Hadley")
charToRaw("Daniel")

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x1, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))
#===========================================================================

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)

#===========================================================================
# date = the number od days since 1970-01-01
# date-time = number of seconds since midnight 1970-01-01
# time = number of seconds since midnight

parse_datetime("2010-10-01T2010")

# If time is ommitted, it will be set to midnight
parse_datetime("20101010")

parse_date("2010-10-01")

library(hms)
parse_time("01:10 am")
parse_time("20:10:01")

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

# Exercises 11.3.5 on website:
# http://r4ds.had.co.nz/data-import.html#exercises-20

# 1. What are the most important arguments to locale()?
# date_names = "en", date_format = "%AD", time_format = "%AT",
# decimal_mark = ".", grouping_mark = ",", tz = "UTC",
# encoding = "UTF-8", asciify = FALSE

# 2. What happens if you try and set decimal_mark and grouping_mark 
# to the same character? What happens to the default value of grouping_mark 
# when you set decimal_mark to ","? What happens to the default value of 
# decimal_mark when you set the grouping_mark to "."?

locale(decimal_mark = ".", grouping_mark = ".")
# get error: Error: `decimal_mark` and `grouping_mark` must be different

locale(decimal_mark = ",")
# if decimal mark is set to the comma then the grouping becomes the period.

locale(grouping_mark = ".")
# if grouping mark is set to the period then the decimal becomes the comma.

# 3. I didn't discuss the date_format and time_format options to locale(). 
# What do they do? Construct an example that shows when they might be useful.

# date_format and time_format allows us to set the data and time formats and
# they parse with flexible YMD parsers and HMS parsers, respetively.

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
parse_date("14 oct. 1979", "%d %b %Y", locale = locale("fr"))

# 4. If you live outside the US, create a new locale object that encapsulates
# the settings for the types of file you read most commonly.
?locale


# 5. What's the difference between read_csv() and read_csv2()?
# read_csv is used for comma-delimited files and read_csv2 for semi-colon.
# Note: read_csv2 useful for countries where comma is used as decimal point.

# 6. What are the most common encodings used in Europe? What are the most 
# common encodings used in Asia? Do some googling to find out.

# UTF-8 is standard now. ASCII has been around forever Others: 
# Western Europe: ISO 8859-1, ISO 8859-15, cp1252
# Eastern Europe: ISO-8859-2, cp1250
# Chinese: GB2312, ... some Korean: EUC-KR, Cyrillic cp1251
?stringi::stri_enc_detect

# 7. Generate the correct format string to parse each of the following dates 
# and times:
  
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1,"%B %d, %Y") 
parse_date(d2, "%Y-%b-%d")
parse_date(d3, "%d-%b-%Y")
parse_date(d4, "%B %d (%Y)")
parse_date(d5, "%m/%d/%y")
parse_time(t1,"%H%M")
parse_time(t2, "%H:%M:%OS %p")
#===========================================================================

guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,651"))
str(parse_guess("2010-10-10"))

challenge <- read_csv(readr_example("challenge.csv"))

# first copy and paste column specification from above error output:
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_integer(),
    y = col_character()
    )
  )
# Now tweak type of x column:
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_character()
  )
)

# first problem is fixed but now look at tail of data:
tail(challenge)
# y is stored as <chr> so we need to specify the y column as well:
challenge <- read_csv(
  readr_example("challenge.csv"),
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
# and check:
tail(challenge)

# last time we got unlucky, looking at one more row would correctly parse
# the data:
challenge2 <- read_csv(
  readr_example("challenge.csv"),
  guess_max = 1001
)

# can diagnose if we read everything in as character vector:
challenge2 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character())
                       )

df <- tribble(
  ~x, ~y, 
  "1", "1.21", 
  "2", "2.32", 
  "3", "4.56" 
  )

df

type_convert(df)
#===========================================================================
write_csv(challenge, "challenge.csv")

# type information is lost when you save to CSV:
read_csv("challenge.csv")

write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

library(feather)
write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")
