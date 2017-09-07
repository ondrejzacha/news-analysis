library(tidyverse)
library(rvest)
library(stringr)
# Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jre1.8.0_144') 
library(RDRPOSTagger)
library(tokenizers)

source("functions.R")
source("scrape_novinky.R")
source("scrape_seznam_search.R")

text_pattern <- "(uprchlí(k|ci)|migranti*)(\\W|$)"

# get urls
novinky_urls <- novinky_get_all_links()

# get named character vectors
novinky_bodies <- sapply(novinky_urls, 
                         function(url) extract_text_from_url(url = url,
                                                             website = "novinky"))

# get corresponding bigram vectors
novinky_bigrams <- lapply(novinky_bodies, extract_bigrams, 
                  method = ifelse(Sys.info()["sysname"] == "Windows",
                                  "tau",
                                  "tokenizers"))

# define model for word annotation
tagger <- rdr_model(language = "Czech", annotation = "UniversalPOS")

# get table of article urls and sentences containing migrant-related words
filtered_sentences <- novinky_bodies %>% 
  tokenize_sentences() %>%
  named_sentences_to_df() %>%
  filter(str_detect(sentence, text_pattern)) %>%
  transform(art_id = group_indices(., article)) %>%
  group_by(art_id) %>%
  mutate(sen_id = seq_along(sentence)) %>%
  ungroup() %>%
  mutate(comp_id = paste0(art_id, "-", sen_id))

# get table of words and their functions, by article
analyzed_sentences <- rdr_pos(tagger, 
                              x = filtered_sentences$sentence,
                              doc_id = filtered_sentences$comp_id) %>%
  as_data_frame() 
analyzed_sentences

interesting_sentences <- analyzed_sentences %>% 
  group_by(doc_id) %>%
  # only filter sentences that contain a matching word as a noun AND a verb
  filter(sum((str_detect(token, text_pattern) & 
                pos == "NOUN") | 
               pos %in% c("VERB", "AUX")) > 1) %>% 
  arrange(doc_id)
interesting_sentences

# look at noun-verb combinations
noun_verb_combinations <- interesting_sentences %>% 
  filter((str_detect(token, text_pattern) & pos == "NOUN") | pos %in% c("VERB", "AUX")) %>% 
  mutate(nouns = sum(pos == "NOUN"),
         noun_order = ifelse(pos == "NOUN",
                             cumsum(pos == "NOUN"),
                             0)) %>%
  arrange(doc_id)
noun_verb_combinations

# process sentences with multiple interesting words
noun_verb_processed <- noun_verb_combinations %>% 
  # replicate groups with n > 2 nouns n times
  .[rep(x = 1:nrow(.), times = .$nouns), ] %>%
  group_by(doc_id, token_id) %>%
  # create a new id for replicated groups 
  mutate(copy_id = ifelse(nouns > 1,
                          seq_along(token_id),
                          -1L)) %>%
  # and make sure there's only one noun in each group
  filter(noun_order != copy_id) 
noun_verb_processed

# select nouns and verbs per group
noun_verb_selected <- noun_verb_processed %>% 
  group_by(doc_id, copy_id) %>%
  mutate(noun_position = sum(token_id * (pos == "NOUN")),
         dist_from_noun = abs(token_id - noun_position),
         closest_verb = dist_from_noun == min(dist_from_noun[pos != "NOUN"])) %>%
  filter(closest_verb == TRUE | pos == "NOUN")
noun_verb_selected

# construct phrases
extracted_phrases <- noun_verb_selected %>%
  summarize(phrase = paste(token[pos == "NOUN"], 
                           # n() - 1 makes sure we always have the second
                           # verb in case there are two with the same distance
                           token[pos %in% c("VERB", "AUX")][n() - 1])) %>%
    select(-copy_id)
extracted_phrases
