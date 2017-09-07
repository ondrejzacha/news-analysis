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
                                  website = c("undefined",
                                              "aktualne", "blesk", "ct24",
                                              "denik", "echo24", "euro",
                                              "idnes", "ihned", "lidovky",
                                              "metro", "nova", "novinky",
                                              "parlamentnilisty", "tyden"),
                                  css = NULL) {
  website <- match.arg(website)
  
  if (website == "undefined" & is.null(css)) {
    stop("Specify css or select a website!")
  } else {
    css <- switch(website,
                  undefined = css,
                  aktualne = "div.clanek-telo p",
                  blesk = "div.content p",
                  ct24 = "div.textcontent p",
                  denik = "div.dv4-clanek-text p",
                  echo24 = "div.article-detail__content p",
                  euro = "div.body p",
                  idnes = "div.text#art-text div.bbtext p",
                  ihned = "div.article-body p",
                  lidovky = "div.text div.bbtext p",
                  metro = "div.text div.bbtext p",
                  nova = "div.article_wrap p",
                  novinky = "div.articleBody p",
                  parlamentnilisty = "section.article-content p",
                  tyden = "div#lightbox-search p"
                  )
  }
  
  try(
    url %>%
      read_html() %>%
      html_nodes(css = css) %>% 
      html_text() %>%
      paste(collapse = "\n")
  )
}
