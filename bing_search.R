library(httr)
library(data.table)

key1 <- "9ccc66b374904ee3b1630f696d7980c6"
key2 <- "3af2651dd6f049d08365aec6ca1f9e6e"


response <- GET(url = 'https://api.cognitive.microsoft.com/bing/v7.0/search?q=drought+site%3atimes.mw', 
               add_headers(`Ocp-Apim-Subscription-Key` = key1))
res <- content(response, encoding = 'json')
res_table <- rbindlist(res$webPages$value)