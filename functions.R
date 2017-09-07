# https://www.w3schools.com/cssref/css_selectors.asp

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

named_sentences_to_df <- function(l) {
  data_frame(article = rep(names(l), lapply(l, length)),
             sentence = unlist(l))
}

extract_text_from_url <- function(url,
                                  website = c("undefined", "novinky", "idnes"),
                                  css = NULL) {
  website <- match.arg(website)
  
  if (website == "undefined" & is.null(css)) {
    stop("Specify css or select a website!")
  } else {
    css <- switch(website,
                  novinky = "div.articleBody p",
                  idnes = "div.text#art-text div.bbtext p")
  }
  
  try(
    url %>%
      read_html() %>%
      html_nodes(css = css) %>% 
      html_text() %>%
      paste(collapse = "\n")
  )
}
