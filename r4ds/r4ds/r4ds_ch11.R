# R for Data Science Chapter 11
# Strings with stringr
# Daniel J. Vera, Ph.D.
library(tidyverse)
library(stringr)

# String Basics ============================================================
string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'
double_quote <- "\""
single_quote <- '\''

x <- c("\"", "\\")
x
writeLines(x)
?'"'
?"'"
x <- "\u00b5"
c("one", "two", "three")

str_length(c("a", "R for data science", NA))
str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")
str_c(" prefix-", c(" a", "b", "c"), "-suffix")

str_c(" prefix-", c(" a", "b", "c"), "-suffix")

name <- "Hadley" 
time_of_day <- "morning" 
birthday <- FALSE 

str_c(
  "Good ", time_of_day, " ", name, 
  if (birthday) " and HAPPY BIRTHDAY", 
  "." 
  )

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

# negative numbers count backwards from end
str_sub(x, -3, -1)

str_sub("a", 1, 5)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

# Turkish has two i's: with and without a dot, and it 
# has a different rule for capitalizing them: 
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x, locale = "haw")

# Exercises 14.2.5 on website:
# http://r4ds.had.co.nz/strings.html#exercises-30

# 1. In code that doesn’t use stringr, you’ll often see paste() and paste0(). 
# What’s the difference between the two functions? What stringr function are 
# they equivalent to? How do the functions differ in their handling of NA?

# paste converts arguments into character strings and concatenates them, 
# seperating using the sep argument.
# paste0 is equivalent to paste(..., sep = "", collapse)
# stringr::str_c is the equivalent in stringr, whenever a missing value
# occurs, it goes through instead of converting to character NA.

paste("foo", "bar")
paste0("foo", "bar")
str_c("foo", "bar")
str_c("foo", "bar", sep = " ")

paste("foo", NA) # converts NA to character "NA" 
str_c("foo", NA) # does not convert NA, you use
str_replace_na("foo", NA) #to drop the NA

# 2. In your own words, describe the difference between the sep and collapse 
# arguments to str_c().

letters

# sep gives the character used to seperate the strings being combined.
str_c("Letter", letters, sep = ": ") #sep is vectorized
# collapse collapses all arguments into single string and gives how to 
# seperate them, sort of a non-vectorized version of sep.
str_c(letters, collapse = "")
str_c(letters, collapse = ", ")

# 3. Use str_length() and str_sub() to extract the middle character from a 
# string. What will you do if the string has an even number of characters?
x <- "foobar"  #even length
y <- "spamegg" #odd length

str_length(x)
str_length(y)
str_sub(x, str_length(x)/2, str_length(x)/2)
str_sub(y, (str_length(y) + 1)/2, (str_length(y) + 1)/2)
# 4. What does str_wrap() do? When might you want to use it?
# ?str_wrap

# 5. What does str_trim() do? What’s the opposite of str_trim()?
# trims whitespace, opposite of trim_pad
str_trim("  String with trailing and leading white space\t")
str_pad("hadley", 7)

# 6. Write a function that turns (e.g.) a vector c("a", "b", "c") into the 
# string a, b, and c. Think carefully about what it should do if given a 
# vector of length 0, 1, or 2.
vector_to_comma_string <- function(a_vect, sep = ", ", last = ", and "){
  if (length(a_vect) == 0) {
    a_vect
  } else {
    str_c(str_c(a_vect[-length(a_vect)], collapse = sep),
          a_vect[length(a_vect)],
          sep = last)
  }
}
vector_to_comma_string(c("a", "b", "c"))
vector_to_comma_string(c("a","b"))
vector_to_comma_string("a")
vector_to_comma_string("")

# Matching Patterns with Regular Expresions ================================
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")

dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b" 
writeLines(x) 
str_view(x, "\\\\")

# Exercises 14.3.1.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-31

# 1. Explain why each of these strings don’t match a \: "\", "\\", "\\\".
# "\" is an escape character for a string
# "\\" is an escape character for a regular expression
# "\\\" The first two backslashes will resolve to a literal backslash in 
# the regular expression, the third will escape the next character. So in 
# the regular expresion, this will escape the next character, a "\".

# 2. How would you match the sequence "'\?
# \\`\\
x <- "a\"'\\b"
writeLines(x)
str_view(x, "\"'\\\\")
# 3. What patterns will the regular expression \..\..\.. match? 
# How would you represent it as a string?
# a dot followed by any character 3 times.
x <- "a.D.J.V"
str_view(x, "\\..\\..\\..")

# Anchors ==================================================================
# ^ matches start of string
# $ matches end of string
# Evan Misshula mnemonic:
# 'if you begin with power (^), you end with money ($).'

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

# Exercises 14.3.2.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-32

# 1. How would you match the literal string "$^$"?
x <- c("a$^$b","$^$")
str_view(x, "^\\$\\^\\$$")
str_view(x, "\\$\\^\\$")

# 2. Given the corpus of common words in stringr::words, create regular 
# expressions that find all words that:
#     a. Start with “y”.
#     b. End with “x”.
#     c. Are exactly three letters long. 
#        (Don’t cheat by using str_length()!)
#     d. Have seven letters or more.
#        Since this list is long, you might want to use the match argument
#        to str_view() to show only the matching or non-matching words.

# a. Start with "y".
str_view(stringr::words, "^y")
str_view(stringr::words, "^y", match = TRUE)
# b. End with "x",
str_view(stringr::words, "x$")
str_view(stringr::words, "x$", match = TRUE)
# c. Are exactly three letters long. 
str_view(stringr::words, "^...$")
str_view(stringr::words, "^...$", match = TRUE)

# d. Have seven letters or more.
#    Since this list is long, you might want to use the match argument
#    to str_view() to show only the matching or non-matching words.
str_view(stringr::words, "^.......", match = TRUE)

# Character Classes and Alternatives =======================================
# \d matches digit
# \s matches whitespace
# [abc] matches a, b, or c.
# [^abc] matches anything except a, b, or c.

str_view(c("grey", "gray"), "gr(e|a)y")

# Exercises 14.3.3.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-33

# 1. Create regular expressions to find all words that:
#     a. Start with a vowel.
#     b. That only contain consonants. 
#        (Hint: thinking about matching “not”-vowels.)
#     c. End with ed, but not with eed.
#     d. End with ing or ize.

# a. Start with a vowel.
str_view(stringr::words, "^[aeiou]", match = TRUE)

# b. That only contain consonants. 
str_view(stringr::words, "^[^aeiou]+$", match = TRUE) # need '+'
#    (Hint: thinking about matching “not”-vowels.)
# c. End with ed, but not with eed.
str_view(stringr::words, "^ed$|[^e]ed$", match = TRUE)
# d. End with ing or ize.
str_view(stringr::words, "i(ng|ze)$", match = TRUE)


# 2. Empirically verify the rule “i before e except after c”.
str_view(stringr::words, "[^c]ie|cei", match = TRUE)

# 3. Is “q” always followed by a “u”?
str_view(stringr::words, "q[^u]", match = TRUE)

# 4. Write a regular expression that matches a word if it’s probably written 
# in British English, not American English.

# 5. Create a regular expression that will match telephone numbers as 
#commonly written in your country.
# "(\d\d\d)\s\d\d\d-\d\d\d\d"

# Repetition ===============================================================
# ?: 0 or 1 match
# +: 1 or more
# *. 0 or more
# {n}: exactly n
# {n, }: n or more
# {, m}: at most m
# {n, m}: between n and m

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x,"CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, "C{2,3}?")
str_view(x, 'C[LX]+?')

# Exercises 14.3.4.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-34

# 1. Describe the equivalents of ?, +, * in {m,n} form.
# ?: {0,1}
# +: {1,}
# *: {0,}

# 2. Describe in words what these regular expressions match: 
# (read carefully to see if I’m using a regular expression or a string 
# that defines a regular expression.)
#     a. ^.*$
#     b.  "\\{.+\\}"
#     c. \d{4}-\d{2}-\d{2}
#     d. "\\\\{4}"

# a. ^.*$
# Start with any character and of any finite length. Any string.

# b.  "\\{.+\\}"
# Any string with curly braces surronding at least one character.

# c. \d{4}-\d{2}-\d{2}
# Four digites, then dash, then 2 digits, then dash, then 2 digits. Could be
# a date in ISO 8601 format, year, month, day.

# d. "\\\\{4}"
# Any string with exactly 4 back slashes.

# 3. Create regular expressions to find all words that:
#     a. Start with three consonants.
#     b. Have three or more vowels in a row.
#     c. Have two or more vowel-consonant pairs in a row.

# a. Start with three consonants.
str_view(stringr::words, "^[^aeiou]{3}", match = TRUE)

# b. Have three or more vowels in a row.
str_view(stringr::words, "[aeiou]{3,}", match = TRUE)

# c. Have two or more vowel-consonant pairs in a row.
str_view(stringr::words, "([aeiou][^aeiou]){2,}", match = TRUE)

# 4. Solve the beginner regexp crosswords at 
# https://regexcrossword.com/challenges/beginner.

# Grouping and Backreferences ==============================================
str_view(fruit, "(..)\\1", match = TRUE)

# Exercises 14.3.5.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-35

# 1. Describe, in words, what these expressions will match:
#     a. (.)\1\1
#     b. "(.)(.)\\2\\1"
#     c. (..)\1
#     d. "(.).\\1.\\1"
#     e. "(.)(.)(.).*\\3\\2\\1"

# a. (.)\1\1
# The same character 3 times in row.
str_view("berrry", "(.)\\1\\1")

# b. "(.)(.)\\2\\1"
str_view(fruit, "(.)(.)\\2\\1", match = TRUE)
# A pair of characters followed by the same characters reversed.

# c. (..)\1
# Repeated pair of letters.

# d. "(.).\\1.\\1"
str_view(fruit, "(.).\\1.\\1", match = TRUE)

# Any character that repeats after any other character and appears 3 times

# e. "(.)(.)(.).*\\3\\2\\1"
str_view(stringr::words, "(.)(.)(.).*\\3\\2\\1", match = TRUE)
# Three characters followed by zero or more characters of any kind
# followed by the same three characters but in reverse order. 


# 2. Construct regular expressions to match words that:
#     a. Start and end with the same character.
#     b. Contain a repeated pair of letters (e.g. “church” contains “ch” 
#        repeated twice.)
#     c. Contain one letter repeated in at least three places (e.g. “eleven”
#        contains three “e”s.)

# a. Start and end with the same character.
str_view(stringr::words, "^(.).*\\1$", match = TRUE)

# b. Contain a repeated pair of letters (e.g. “church” contains “ch” 
#        repeated twice.)
str_view(stringr::words, "(..).*\\1", match = TRUE)

# c. Contain one letter repeated in at least three places (e.g. “eleven”
#    contains three “e”s.)
str_view(stringr::words, "(.).*\\1.*\\1", match = TRUE)

# Tools ====================================================================
# Detect Matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")

# How many common words start with a t?
sum(str_detect(words, "^t"))
# What proportion of common words end wtih a vowel?
mean(str_detect(words, "[aeiou]$"))

# Two ways to find all words that don't contain vowels:

# Method 1: Find all words containing at least one vowel and negate
no_vowels_1 <- !str_detect(words,"[aeiou]")
# Method 2: Find all words consisting only of consonants:
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]
str_subset(words, "x$")

df <- tibble(
  word = words,
  i = seq_along(word)
)

df %>%
  filter(str_detect(words, "x$"))

str_count(x, "a")
mean(str_count(words, "[aeiou]")) # average proportion of vowels per word

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  ) 

str_count("abababa", "aba")
str_view_all("abababa", "aba")

# Exercises 14.4.2 on website:
# http://r4ds.had.co.nz/strings.html#exercises-36

# 1. For each of the following challenges, try solving it by using both a 
# single regular expression, and a combination of multiple str_detect() 
# calls.

#     a. Find all words that start or end with x.
x <- str_detect(words, "(^x|x$)")
y <- str_detect(words, "^x") | str_detect(words, "x$") 
identical(x, y)
#     b. Find all words that start with a vowel and end with a consonant.
x <- str_detect(words, "(^[aeiou].*[^aeiou]$)")
y <- str_detect(words, "^[aeiou]") 
z <- str_detect(words, "[^aeiou]$")
identical(words[x], words[y & z])
#     c. Are there any words that contain at least one of each different 
#        vowel?
has_a <- str_detect(words, "a+")
has_e <- str_detect(words, "e+")
has_i <- str_detect(words, "i+")
has_o <- str_detect(words, "o+")
has_u <- str_detect(words, "u+")

words[has_a & has_e & has_i & has_o & has_u]

# No words have at least one of each vowel.
# Hard to do as singular regexp.

#     d. What word has the highest number of vowels? What word has the 
#        highest proportion of vowels? (Hint: what is the denominator?)
words_df <- tibble(
  word = words,
  i = seq_along(word)
)
str_count(words, "[aeiou]")

words_df %>%
  mutate(
    num_vowels = str_count(word, "[aeiou]")
  ) %>%
  arrange(desc(num_vowels))

# appropriate has the highest number of vowels
words_df %>%
  mutate(
    num_vowels = str_count(word, "[aeiou]"),
    word_length = str_length(word),
    prop_vowels = num_vowels / word_length
    ) %>%
  arrange(desc(prop_vowels))

# a has the highest proportion of vowels (100%) followed by area at 75%

# Extract Matches
stringr::sentences
length(sentences)
head(sentences)

# Imagine we want all sentences that contain a color.

colors <- c(
  "red", "orange", "yellow", "green", "blue", "purple"
)
color_match <- str_c(colors, collapse = "|")

has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
matches
more <- sentences[str_count(sentences, color_match) > 1]
str_extract(more, color_match)
str_extract_all(more, color_match) # returns list
str_extract_all(more, color_match, simplify = TRUE) # returns matrix
x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

# Exercises 14.4.3.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-37

# 1. In the previous example, you might have noticed that the regular 
# expression matched “flickered”, which is not a colour. 
# Modify the regex to fix the problem.

# First method:
color_fix <- c(
  "\\sred\\s", "orange", "yellow", "green", "blue", "purple"
)
color_match_fix <- str_c(color_fix, collapse = "|")
str_view_all(more, color_match_fix)

# this works but keeps spaces around "red". It also misses the last "red" in
# third sentence. jrnold's solution is betterusing the word boundary 
# regex \b:
color_fix2 <- str_c("\\b", str_c(colors, collapse = "|"), "\\b")
str_view_all(more, color_fix2)


# 2. From the Harvard sentences data, extract:
#     a. The first word from each sentence.
str_extract(sentences, "[a-zA-Z]+") %>% head()
#     b. All words ending in ing.
pattern <- "\\b[a-zA-Z]+ing\\b"
sentences_ing <- str_subset(sentences, pattern)
str_extract_all(sentences_ing, pattern)
#     c. All plurals.
pattern2 <- "\\b[a-zA-Z]{3,}s\\b"
# From jrnold's notes to do this correctly requires linguistic information. 
# jrnold defines word ending in an “s” and with 3 or more characters as 
# plural; this elimantes words like "is" and "gas" but not words like "his"
# or "hostess".
sentences_s <- str_subset(sentences, pattern2)
str_extract_all(sentences_s, pattern2)

# Grouped Matches

# Imagine we want to extract nouns from sentences.
# Heuristic: look for any word that comes after "a" or "the".
noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>%
  str_extract(noun)

has_noun %>%
  str_match(noun)

tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

# Exercises 14.4.4.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-38

# 1. Find all words that come after a “number” like “one”, “two”, “three” 
# etc. Pull out both the number and the word.
after_number <- "(one|two|three|four|five|six|seven|eight|nine|ten) ([^ ]+)"

sentences[str_detect(sentences, after_number)] %>%
  str_extract(after_number)

# 2. Find all contractions. Separate out the pieces before and after the 
# apostrophe.
contractions <- "[A-Za-z]+'[A-Za-z]"
sentences[str_detect(sentences, contractions)] %>%
  str_extract(contractions)

# Replacing Matches

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

# Example of flipping the order of second and third words from textbook:
sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
  head(5)

# Exercises 14.4.5.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-39

# 1. Replace all forward slashes in a string with backslashes.
r4ds_website <- "http://r4ds.had.co.nz/strings.html#grouped-matches"
str_replace_all(r4ds_website, c("/" = "\\\\"))
# I do not know how to get a single backslash.

# 2. Implement a simple version of str_to_lower() using replace_all().
name <- "DanieL J VerA"
str_to_lower(name)
str_replace_all(name, c("D" = "d", "L" = "l", "V" = "v", "A" = "a"))

# 3. Switch the first and last letters in words. Which of those strings 
# are still words?
words
flipped <- words %>%
  str_replace("^(.)(.*)(.)$", "\\3\\2\\1")

words[words %in% flipped]

# Splitting
sentences %>%
  head(5) %>%
  str_split(" ")

"a|b|c|d" %>%
  str_split("\\|") %>%
  .[[1]]

sentences %>%
  head(5) %>%
  str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)

x <- "This is a sentence. This is another sentence."
str_view_all(x, boundary("word"))

str_split(x, " ")[[1]]
str_split(x, boundary("word"))[[1]]

# Exercises 14.4.6.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-40

# 1. Split up a string like "apples, pears, and bananas" into individual 
# components.
a_string <- "apples, pears, and bananas"
str_split(a_string, boundary("word"))
str_split(a_string, " ")

# 2. Why is it better to split up by boundary("word") than " "?
# In above exercise, when splitting with " ", we include commas, e.g.
# "apples," but boundary("word") gives us the word.

# 3. What does splitting with an empty string ("") do? 
# Experiment, and then read the documentation.
str_split(a_string, "")
# gives us each character.
# From documentation ?str_split " An empty pattern, "", is equivalent 
# to boundary("character").
str_split(a_string, boundary("character"))

# Find Matches
name
str_locate(name, "a")
names <- c("Daniel", "J", "Vera")
str_locate_all(names, "a")

str_sub(name, start = 1L, end = str_locate(name, "ni"))

# Other Types of Pattern ===================================================
# The regular call:
str_view(fruit, "nana")
# is shorthand for
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

phone <- regex("
  \\(?       # optional opening parens 
  (\\d{3}) # area code 
  [)- ]?     # optional closing parens, dash, or space 
  (\\d{3}) # another three numbers 
  [ -]?      # optional space or dash 
  (\\d{3}) # three more numbers 
  ", comments = TRUE) 
str_match("514-791-8141", phone)

microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")), 
  regex = str_detect(sentences, "the"), 
  times = 20
)                             

a1 <- "\u00e1"
a2 <- "a\u0301" 
c( a1, a2)
a1 == a2
str_detect(a1, fixed(a2))
str_detect(a1, coll(a2))

i <- c(" I", "İ", "i", "ı")
i
str_subset(i, coll("i", ignore_case = TRUE))
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

stringi::stri_locale_info()

# Exercises 14.5.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-41

# 1. How would you find all strings containing \ with regex() vs. 
# with fixed()?
str_subset(c("a\\b", "ab"), "\\\\")
str_subset(c("a\\b", "ab"), fixed("\\"))

# 2. What are the five most common words in sentences?
str_extract_all(sentences, boundary("word")) %>%
  unlist() %>%
  str_to_lower() %>%
  tibble() %>%
  set_names("word") %>%
  group_by(word) %>%
  count(sort = TRUE) %>%
  head(5)
# Other Uses of Regular Expressions ========================================

apropos("replace")
head(dir(pattern = "\\.Rmd$"))

# stringi ==================================================================
# Exercises 14.7.1 on website:
# http://r4ds.had.co.nz/strings.html#exercises-42

# 1. Find the stringi functions that:
#     a. Count the number of words.
# Use apropos after loading stringi. stri_count_words
#     b. Find duplicated strings.
# stri_duplicated
#     c. Generate random text.
# stri_rand_strings

# 2. How do you control the language that stri_sort() uses for sorting?
# use locale argument.
