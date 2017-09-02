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

named_sentences_to_dt <- function(l) {
  data.table(article = rep(names(l), lapply(l, length)),
             sentence = unlist(l))
}
