library(tidyverse)
library(tidytext)
library(rvest)

## set some locale that works  # https://github.com/juliasilge/tidytext/issues/63
Sys.setlocale(locale = "Czech_Czech Republic.1250")  # probably not this one 

# https://www.w3schools.com/cssref/css_selectors.asp

HTM <- read_html("https://www.novinky.cz/domaci/445854-cesko-podalo-zalobu-na-evropskou-smernici-o-zbranich.html")

## get text from html
body <- html_nodes(HTM, "div.articleBody p") %>% 
  html_text()  # %>% paste(collapse = " ) 

perex <- html_nodes(HTM, "div#articleHeaderBig p") %>% 
  html_text()

title <- html_nodes(HTM, "div#articleHeaderBig h1") %>% 
  html_text()

## get ngrams
bigrams <- body %>% 
  data_frame(text = .) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE)
bigrams
