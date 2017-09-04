library(tidyverse)
# library(tidytext)
library(rvest)
library(stringr)
# library(data.table)
library(RDRPOSTagger)
library(tokenizers)

source("functions.R")

# https://www.w3schools.com/cssref/css_selectors.asp

text_pattern <- "(uprchlí(k|ci)|migranti*)(\\W|$)"

urls <- c("https://www.novinky.cz/zahranicni/446613-z-valkou-zmitaneho-jizniho-sudanu-uteklo-do-ugandy-uz-milion-lidi-osn-zada-o-pomoc.html",
          "https://www.novinky.cz/zahranicni/blizky-a-stredni-vychod/446434-v-obklicenem-tall-afaru-neni-jidlo-utok-na-is-zatim-nezacne.html",
          "https://www.novinky.cz/zahranicni/blizky-a-stredni-vychod/446329-islamsky-stat-v-syrii-vyzval-k-povinnemu-dzihadu-v-rakce-se-pokusil-o-protiutok.html",
          "https://www.novinky.cz/zahranicni/evropa/446274-nemecke-soudy-jsou-zavalene-zalobami-zadatelu-o-azyl.html",
          "https://www.novinky.cz/zahranicni/evropa/446264-nevyplacet-eurodotace-kvuli-odmitani-migrantu-nesmysl-tvrdi-merkelova.html",
          "https://www.novinky.cz/zahranicni/svet/445967-paseraci-u-jemenu-vyhodili-ze-clunu-do-vln-na-180-lidi-50-migrantu-je-nezvestnych.html",
          
          "https://www.novinky.cz/zahranicni/447546-proverujme-migranty-uz-v-africe-volaji-po-prisnejsich-pravidlech-evropsti-lidri.html",
          "https://www.novinky.cz/zahranicni/447535-zajedte-si-na-svatky-domu-a-pak-se-vratte-nabizeji-turci-syrskym-uprchlikum.html",
          "https://www.novinky.cz/zahranicni/447477-migranti-vymenili-calais-za-bilbao-do-anglie-se-zkouseji-dostat-i-dvakrat-denne.html",
          "https://www.novinky.cz/zahranicni/447029-cerne-more-se-stava-novou-migracni-trasou-desi-se-rumunsko.html",
          "https://www.novinky.cz/zahranicni/447021-zajmy-uprchliku-jsou-dulezitejsi-nez-narodni-bezpecnost-tvrdi-papez-frantisek.html",
          "https://www.novinky.cz/zahranicni/446613-z-valkou-zmitaneho-jizniho-sudanu-uteklo-do-ugandy-uz-milion-lidi-osn-zada-o-pomoc.html",
          "https://www.novinky.cz/zahranicni/446434-v-obklicenem-tall-afaru-neni-jidlo-utok-na-is-zatim-nezacne.html",
          "https://www.novinky.cz/zahranicni/446329-islamsky-stat-v-syrii-vyzval-k-povinnemu-dzihadu-v-rakce-se-pokusil-o-protiutok.html",
          "https://www.novinky.cz/zahranicni/446274-nemecke-soudy-jsou-zavalene-zalobami-zadatelu-o-azyl.html")

# get named character vectors
bodies <- sapply(urls, extract_text_from_url_novinky)

# get corresponding bigram vectors
bigrams <- lapply(bodies, extract_bigrams, 
                  method = ifelse(Sys.info()["sysname"] == "Windows",
                                  "tau",
                                  "tokenizers"))

# define model for word annotation
tagger <- rdr_model(language = "Czech", annotation = "UniversalPOS")

# get table of article urls and sentences containing migrant-related words
filtered_sentences <- bodies %>% 
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
               pos == "VERB") > 1) %>% 
  arrange(doc_id)
interesting_sentences

# look at noun-verb combinations
noun_verb_combinations <- interesting_sentences %>% 
  filter((str_detect(token, text_pattern) & pos == "NOUN") | pos == "VERB") %>% 
  arrange(doc_id)
noun_verb_combinations

noun_verb_selected <- noun_verb_combinations %>%
  group_by(doc_id) %>%
  mutate(noun_position = sum(token_id * (pos == "NOUN")),
         dist_from_noun = abs(token_id - noun_position),
         closest_verb = dist_from_noun == min(dist_from_noun[pos != "NOUN"])) %>%
  filter(closest_verb == TRUE | pos == "NOUN")
noun_verb_selected

extracted_phrases <- noun_verb_selected %>%
  summarize(phrase = paste(token[pos == "NOUN"], 
                           token[pos == "VERB"]))
# this only works with one verb and one noun per doc_id!
extracted_phrases
