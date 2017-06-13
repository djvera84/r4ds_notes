# R for Data Science Chapter 12
# Factors with forcats
# Daniel J. Vera, Ph.D.
library(tidyverse)
library(forcats)

# Creating Factors =========================================================
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar") # typos are easy mistakes
sort(x1)

# To create factor, fist create 'levels'
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y1
sort(y1)
y2 <- factor(x2, levels = month_levels)
y2

y2 <- parse_factor(x2, levels = month_levels)

factor(x1)

f1 <- factor(x1, levels = unique(x1))
f1

f2 <- x1 %>% factor() %>% fct_inorder()
f2

levels(f2)

# General Social Survey ====================================================
gss_cat

gss_cat %>%
  count(race)

ggplot(gss_cat, aes(race)) +
  geom_bar()

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

# Exercises 15.3.1 on website:
# http://r4ds.had.co.nz/factors.html#exercise
# 1. Explore the distribution of rincome (reported income). What makes the 
# default bar chart hard to understand? How could you improve the plot?
ggplot(gss_cat, aes(rincome)) +
  geom_bar()

# axis labels are hard to read. Consider flipping coordinates:
ggplot(gss_cat, aes(rincome)) +
  geom_bar() +
  coord_flip()

# 2. What is the most common relig in this survey? 
# What’s the most common partyid?
gss_cat %>%
  group_by(relig) %>%
  count() %>%
  arrange(desc(n))

gss_cat %>%
  count(relig, sort = TRUE)
# Protestant, followed by Catholic and None.

gss_cat %>%
  group_by(partyid) %>%
  count() %>%
  arrange(desc(n))

gss_cat %>%
  count(partyid, sort = TRUE)

# Independent followed by Not str democrat.

# 3. Which relig does denom (denomination) apply to? How can you find out 
# with a table? How can you find out with a visualisation?

gss_cat %>% 
  group_by(relig, denom) %>%
  count() %>%
  arrange(desc(n))

levels(gss_cat$denom)

gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

# Modifying Factor Order ===================================================
relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig, aes(tvhours, relig)) + geom_point()

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point()

relig %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome <- gss_cat %>%
  group_by(rincome) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(
  rincome,
  aes(age, fct_reorder(rincome,age))
  ) + geom_point()
    
ggplot(
  rincome,
  aes(age, fct_relevel(rincome, "Not applicable"))
) +
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, color = marital)) +
  geom_line(na.rm = TRUE)

ggplot(
  by_age,
  aes(age, prop, color = fct_reorder2(marital, age, prop))
) + 
  geom_line() +
  labs(color = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

# Exercises 15.4.1 on website:
# http://r4ds.had.co.nz/factors.html#exercise-43
# 1. There are some suspiciously high numbers in tvhours. Is the mean a 
# good summary?
summary(gss_cat$tvhours)

gss_cat %>%
  filter(!is.na(tvhours)) %>%
  ggplot(aes(x = tvhours)) +
  geom_histogram(binwidth = 1)

relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = median(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) + 
  geom_point()

# can also use median

# 2. For each factor in gss_cat identify whether the order of the levels 
# is arbitrary or principled.
levels(gss_cat$marital)
summary(gss_cat$marital)
# arbitrary
levels(gss_cat$race)
summary(gss_cat$race)
# ordered by observation
levels(gss_cat$rincome)
summary(gss_cat$rincome)
# principaled
levels(gss_cat$partyid)
summary(gss_cat$partyid)
# principaled by politcal spectrum
levels(gss_cat$relig)
summary(gss_cat$relig)
# arbitrary
levels(gss_cat$denom)
summary(gss_cat$denom)
# arbitrary

# 3. Why did moving “Not applicable” to the front of the levels move it 
# to the bottom of the plot?
# Gives "Not applicable" an integer value of 1.

# Modifying Factor Levels ==================================================
gss_cat %>% count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid, 
    "Republican, strong"    = "Strong republican", 
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep", 
    "Independent, near dem" = "Ind,near dem", 
    "Democrat, weak"        = "Not str democrat", 
    "Democrat, strong"      = "Strong democrat" 
    )) %>% 
  count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid, 
    "Republican, strong"    = "Strong republican", 
    "Republican, weak"      = "Not str republican",
    "Independent, near rep" = "Ind,near rep", 
    "Independent, near dem" = "Ind,near dem", 
    "Democrat, weak"        = "Not str democrat", 
    "Democrat, strong"      = "Strong democrat",
    "Other"                 = "No answer",
    "Other"                 = "Don't know",
    "Other"                 = "Other party"
    )) %>% 
  count(partyid)

gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
    other = c("No answer", "Don't know", "Other party"), 
    rep = c("Strong republican", "Not str republican"), 
    ind = c("Ind,near rep", "Independent", "Ind,near dem"), 
    dem = c("Not str democrat", "Strong democrat")
    )) %>% 
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)

# Exercises 15.5.1 on website:
# http://r4ds.had.co.nz/factors.html#exercise-44
# 1. How have the proportions of people identifying as Democrat, Republican, 
# and Independent changed over time?
gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
   other = c("No answer", "Don't know", "Other party"), 
   rep = c("Strong republican", "Not str republican"), 
   ind = c("Ind,near rep", "Independent", "Ind,near dem"), 
   dem = c("Not str democrat", "Strong democrat")
  )) %>% 
  group_by(year) %>%
  count(partyid) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(year, p, color = fct_reorder2(partyid, year, p))) +
  geom_line() +
  geom_point() +
  labs(color = "Party ID")

# 2. How could you collapse rincome into a small set of categories?
levels(gss_cat$rincome)

gss_cat %>% 
  mutate(rincome = fct_collapse(rincome,
   na = c("No answer", "Don't know", "Refused", "Not applicable"), 
   high = c("$25000 or more"), 
   mid = c("$20000 - 24999", "$15000 - 19999", "$10000 - 14999"), 
   low = c("$8000 to 9999", "$7000 to 7999", "$6000 to 6999",
           "$5000 to 5999","$4000 to 4999",  "$3000 to 3999", 
           "$1000 to 2999",  "Lt $1000"))
   ) %>% 
  count(rincome)

# jrnold's solution:
library(stringr)
gss_cat %>%
  mutate(rincome = 
           fct_collapse(
             rincome,
             `Unknown` = 
               c("No answer", "Don't know", "Refused", "Not applicable"),
             `Lt $5000` = 
               c("Lt $1000", str_c("$", c("1000", "3000", "4000"),
                                    " to ", c("2999", "3999", "4999"))),
             `$5000 to 10000` = str_c("$", c("5000", "6000", "7000", "8000"),
                                      " to ", c("5999", "6999", "7999", "9999"))
           )) %>%
  ggplot(aes(x = rincome)) +
  geom_bar() + 
  coord_flip()
