library(tidyverse)
library(tidytext)
library(rvest)

## set some locale that works  # https://github.com/juliasilge/tidytext/issues/63
# Sys.setlocale(locale = "Czech_Czech Republic.1250")  # probably not this one, not neccessary when UTF is default

# https://www.w3schools.com/cssref/css_selectors.asp

urls <- c("https://www.novinky.cz/zahranicni/446613-z-valkou-zmitaneho-jizniho-sudanu-uteklo-do-ugandy-uz-milion-lidi-osn-zada-o-pomoc.html",
          "https://www.novinky.cz/zahranicni/blizky-a-stredni-vychod/446434-v-obklicenem-tall-afaru-neni-jidlo-utok-na-is-zatim-nezacne.html",
          "https://www.novinky.cz/zahranicni/blizky-a-stredni-vychod/446329-islamsky-stat-v-syrii-vyzval-k-povinnemu-dzihadu-v-rakce-se-pokusil-o-protiutok.html",
          "https://www.novinky.cz/zahranicni/evropa/446274-nemecke-soudy-jsou-zavalene-zalobami-zadatelu-o-azyl.html",
          "https://www.novinky.cz/zahranicni/evropa/446264-nevyplacet-eurodotace-kvuli-odmitani-migrantu-nesmysl-tvrdi-merkelova.html",
          "https://www.novinky.cz/zahranicni/svet/445967-paseraci-u-jemenu-vyhodili-ze-clunu-do-vln-na-180-lidi-50-migrantu-je-nezvestnych.html")


bodies = sapply(urls, function(url){  # all the bodies in a list
  try(
    url %>%
      read_html() %>%
      html_nodes("div.articleBody p") %>% 
      html_text()
  )
})


bigrams = lapply(bodies, function(vec){vec %>% data_frame(text = .) %>%         # all the bigrams in a list
    unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE)})

#HTM <- read_html("https://www.novinky.cz/zahranicni/evropa/446841-ridic-narazil-do-dvou-autobusovych-zastavek-v-marseille-jedna-mrtva.html")

## get text from html
#body <- html_nodes(HTM, "div.articleBody p") %>%  html_text()  # %>% paste(collapse = " ) 

# perex <- html_nodes(HTM, "div#articleHeaderBig p") %>%  html_text()

#title <- html_nodes(HTM, "div#articleHeaderBig h1") %>%html_text()
