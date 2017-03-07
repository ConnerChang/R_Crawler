library(httr)
library(jsonlite)
library(magrittr)

get_pchome_items <- function(query, page) {
    # make a GET request
    url <- "http://ecshweb.pchome.com.tw/search/v3.3/all/results?"
    res <- GET(url = url,
               query = list(
                   q = query,
                   page = page
               ))
    # get and parse data
    dat <- res %>% 
        content(as = "text", encoding = "UTF-8") %>% 
        fromJSON() %>% 
        .$prods 
    
    # return NULL if no data was retrieved
    if (is.null(dat)) {
        message(sprintf("No data in query %s in page %d", query, page))
        return(data.frame())
    }
    # clean data, suppose that we need only name, describe, and price
    dat <- dat %>% 
        .[, 5:7]
    dat$price <- as.double(dat$price)
    
    return(dat)
}