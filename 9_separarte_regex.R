##  PROBLEMS WITH THIS CODE. NEEDS FIXING

library(tidyverse)
library(dslabs)
library(rvest)

# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)
tab

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters,
# but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# Case 1 24.7 (BOOK)
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

# Cases 2 and 4  24.7
str_replace(s, "^([56])'?$", "\\1'0")

# Case 3  24.7
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# Case 5  24.7
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

# Trimming
s <- "Hi "
cat(s)
identical(s, "Hi")
str_trim("5 ' 9 ")

# To upper and to lower case
s <- c("Five feet eight inches")
str_to_lower(s)

# Putting it into a function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

# We can also write a function that converts words to numbers:
  words_to_numbers <- function(s){
    str_to_lower("s") %>%  
      str_replace_all("zero", "0") %>%
      str_replace_all("one", "1") %>%
      str_replace_all("two", "2") %>%
      str_replace_all("three", "3") %>%
      str_replace_all("four", "4") %>%
      str_replace_all("five", "5") %>%
      str_replace_all("six", "6") %>%
      str_replace_all("seven", "7") %>%
      str_replace_all("eight", "8") %>%
      str_replace_all("nine", "9") %>%
      str_replace_all("ten", "10") %>%
      str_replace_all("eleven", "11")
  }
  
  not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
    inches <- suppressWarnings(as.numeric(x))
    ind <- !is.na(inches) & 
      ((inches >= smallest & inches <= tallest) |
         (inches/2.54 >= smallest & inches/2.54 <= tallest))
    !ind
  }

#  Now we can see which problematic entries remain:
  converted <- problems %>% words_to_numbers %>% convert_format
  remaining_problems <- converted[not_inches_or_cm(converted)]
  pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
  index <- str_detect(remaining_problems, pattern)
  remaining_problems[!index]
  
# Putting it All Together
  pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
  
  smallest <- 50
  tallest <- 84
  new_heights <- reported_heights %>% 
    mutate(original = height, 
           height = words_to_numbers(height) %>% convert_format()) %>%
    extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
    mutate_at(c("height", "feet", "inches"), as.numeric) %>%
    mutate(guess = 12*feet + inches) %>%
    mutate(height = case_when(
      !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
      !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
      !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
      !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
      TRUE ~ as.numeric(NA))) %>%
    select(-guess)
  
#  We can check all the entries we converted using the following code:
    
    new_heights %>%
    filter(not_inches(original)) %>%
    select(original, height) %>% 
    arrange(height) %>%
    View()
    
    new_heights %>% arrange(height) %>% head(n=7)
   
  
  