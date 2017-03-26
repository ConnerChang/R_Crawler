library(magrittr)
library(httr)
library(rvest)

## Build Connection
url <- "https://www.ptt.cc/bbs/Gossiping/index.html"
res <- GET(url, 
           set_cookies(over18 = "1"))  # set cookie

## Get post titles in the index page 
res %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    read_html() %>% 
    html_nodes(css = ".title a") %>% 
    html_text()
