library(tidyverse)
library(tidytext)
library(rvest)

# https://www.w3schools.com/cssref/css_selectors.asp

urls <- c("https://www.novinky.cz/zahranicni/446613-z-valkou-zmitaneho-jizniho-sudanu-uteklo-do-ugandy-uz-milion-lidi-osn-zada-o-pomoc.html",
          "https://www.novinky.cz/zahranicni/blizky-a-stredni-vychod/446434-v-obklicenem-tall-afaru-neni-jidlo-utok-na-is-zatim-nezacne.html",
          "https://www.novinky.cz/zahranicni/blizky-a-stredni-vychod/446329-islamsky-stat-v-syrii-vyzval-k-povinnemu-dzihadu-v-rakce-se-pokusil-o-protiutok.html",
          "https://www.novinky.cz/zahranicni/evropa/446274-nemecke-soudy-jsou-zavalene-zalobami-zadatelu-o-azyl.html",
          "https://www.novinky.cz/zahranicni/evropa/446264-nevyplacet-eurodotace-kvuli-odmitani-migrantu-nesmysl-tvrdi-merkelova.html",
          "https://www.novinky.cz/zahranicni/svet/445967-paseraci-u-jemenu-vyhodili-ze-clunu-do-vln-na-180-lidi-50-migrantu-je-nezvestnych.html")

extract_text_from_url <- function(url, css = "div.articleBody p") {
  try(
    url %>%
      read_html() %>%
      html_nodes(css = css) %>% 
      html_text() %>%
      paste(collapse = "\n")
  )
}

extract_bigrams <- function(text, method = c("tau", "tokenizers")) {
  method <- match.arg(method)
  if (method == "tau") {
    require("tau")
    bigrams <- text %>% 
      tau::textcnt(n = 2, split = " ", method = "string") %>%
      names()
  } else {
    bigrams <- text %>% 
      data_frame(text = .) %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2, collapse = FALSE) %>%
      `[[`("bigram")
  }
  bigrams
}

# get named character vectors
bodies <- sapply(urls, extract_text_from_url)

# get corresponding bigram vectors
bigrams <- lapply(bodies, extract_bigrams, 
                  method = ifelse(Sys.info("sysname") == "Windows",
                                  "tau",
                                  "tokenizers"))
