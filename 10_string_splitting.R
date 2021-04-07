library(tidyverse)
library(dslabs)
library(rvest)

# read raw murders data line by line
 filename <- system.file("extdata/murders.csv", package = "dslabs")
 lines <- readLines(filename)
 lines %>% head()

# split at commas with str_split function, remove row of column names
 x <- str_split(lines, ",")
 x %>% head()
 col_names <- x[[1]]
 x <- x[-1]

# extract first element of each list entry
 library(purrr)
 map(x, function(y) y[1]) %>% head()
 map(x, 1) %>% head()

 # extract columns 1-5 as characters, then convert to proper format -
 # NOTE: DIFFERENT FROM VIDEO
 dat <- data.frame(parse_guess(map_chr(x, 1)),
                   parse_guess(map_chr(x, 2)),
                   parse_guess(map_chr(x, 3)),
                   parse_guess(map_chr(x, 4)),
                   parse_guess(map_chr(x, 5))) %>%
   setNames(col_names)

 dat %>% head

 # more efficient code for the same thing
 dat <- x %>%
   transpose() %>%
   map( ~ parse_guess(unlist(.))) %>%
   setNames(col_names) %>%
   as.data.frame()
 head(dat)

 # the simplify argument makes str_split return a matrix instead of a list
 x <- str_split(lines, ",", simplify = TRUE)
 col_names <- x[1,]
 x <- x[-1,]
 x %>% as_data_frame() %>%
   setNames(col_names) %>%
   mutate_all(parse_guess)

data("research_funding_rates")
research_funding_rates

library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
txt

print(raw_data_research_funding_rates <- txt[2])
data("raw_data_research_funding_rates")
raw_data_research_funding_rates%>% head
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab %>% head
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)