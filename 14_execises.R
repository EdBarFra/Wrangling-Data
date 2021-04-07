library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata
book <- gutenberg_metadata %>%
         filter(str_detect(title, "Pride and Prejudice"))
book

gutenberg_works(author == "Austen, Jane",
                title  == "Pride and Prejudice",
                languages = "en",
                only_text = TRUE,
                rights = c("Public domain in the USA.", "None"),
                distinct = TRUE,
                all_languages = FALSE,
                only_languages = TRUE)

#browseVignettes(package = "tidytext")
pride <- gutenberg_download(1342) %>%
         unnest_tokens(word, text) %>%
         count(word)
         
 



# words <- pride %>%  unnest_tokens(word, text)
# head(words)
# class(words)
# nrow(words)
# count(words, vars = "gutenberg_id text")
# stop_words
# words <- words %>% filter(!word %in% stop_words$word ) 
# words
# count(words, vars = "gutenberg_id text")
