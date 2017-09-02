library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)
library(data.table)
library(RDRPOSTagger)
library(tokenizers)

source("functions.R")

# https://www.w3schools.com/cssref/css_selectors.asp

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
bodies <- sapply(urls, extract_text_from_url)

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
  named_sentences_to_dt() %>%
  filter(str_detect(sentence, "(uprchl|migrant)"))

# get table of words and their functions, by article
analyzed_sentences <- rdr_pos(tagger, 
                              x = filtered_sentences$sentence,
                              doc_id = filtered_sentences$article) %>%
  data.table()
