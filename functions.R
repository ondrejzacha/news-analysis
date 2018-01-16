# https://www.w3schools.com/cssref/css_selectors.asp 

extract_text_from_url <- function(url,
                                  website = c("undefined",
                                              "aktualne", "blesk", "ct24",
                                              "denik", "echo24", "euro",
                                              "idnes", "ihned", "lidovky",
                                              "metro", "nova", "novinky",
                                              "parlamentnilisty", "tyden"),
                                  css = NULL,
                                  verbose = FALSE,
                                  sleep_secs = 1) {
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
  
  text <- try(
    url %>%
      url_connection(handle = new_handle()) %>%
      read_html() %>%
      html_nodes(css = css) %>% 
      html_text() %>%
      paste(collapse = "\n")
  )
  if (verbose) print(substr(text, 1, 50))
  Sys.sleep(sleep_secs)
  text
}

url_connection <- function(url, 
                           handle, 
                           user_agent_strings = getOption("user_agent_strings"),
                           sleep_sec_interval = getOption("sleep_sec_interval", c(1, 15))) {
  require("curl")
  if (is.null(user_agent_strings)) {
    user_agent_options <- c("Mozilla/5.0 (Windows NT 6.3; Trident/7.0; rv:11.0) like Gecko")
  }
  # do nothing for a random amount of seconds (within interval)
  Sys.sleep(runif(1, sleep_sec_interval[1], sleep_sec_interval[2]))
  # pretend to be a random browser
  handle_setheaders(handle = handle, "User-Agent" = sample(user_agent_strings, 1))
  # return connection
  curl(url = url, open = "rb", handle = handle)
}
