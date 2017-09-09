library(tidyverse)
library(rvest)
library(stringr)
# Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_144') 
library(RDRPOSTagger)
library(tokenizers)

source("config.R")
source("functions.R")
source("scrape_novinky.R")
source("scrape_seznam_search.R")

text_pattern <- "(uprchlí(k|ci)|migranti*)(\\W|$)"

# define model for word annotation
tagger <- rdr_model(language = "Czech", annotation = "UniversalPOS")


# get urls
novinky_urls <- novinky_get_all_links(query = "uprchlíci",
                                      exact_phrase = 0,
                                      section = -1,
                                      exclude = "",
                                      date_from = "1.9.2016",
                                      date_to = "",
                                      sleep_secs = 5)

# get named character vectors
novinky_bodies <- sapply(novinky_urls, 
                         function(url) extract_text_from_url(url = url,
                                                             website = "novinky",
                                                             sleep_secs = 5))

novinky_phrases <- extract_phrases_from_bodies(novinky_bodies)

# # get corresponding bigram vectors
# novinky_bigrams <- lapply(novinky_bodies, extract_bigrams, 
#                   method = ifelse(Sys.info()["sysname"] == "Windows",
#                                   "tau",
#                                   "tokenizers"))




