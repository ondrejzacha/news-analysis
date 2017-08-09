library(tidyverse)
library(tidytext)
library(rvest)

# https://www.w3schools.com/cssref/css_selectors.asp

HTM <- read_html("https://www.novinky.cz/domaci/445854-cesko-podalo-zalobu-na-evropskou-smernici-o-zbranich.html", encoding = "UTF-8")

body <- html_nodes(HTM, "div.articleBody p") %>% 
  html_text() %>%
  paste(collapse = " ")

perex <- html_nodes(HTM, "div#articleHeaderBig p") %>% 
  html_text()

title <- html_nodes(HTM, "div#articleHeaderBig h1") %>% 
  html_text()