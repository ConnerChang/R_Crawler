library(magrittr)
library(httr)
library(rvest)

get_stock_price <- function(stock_id) {
    url <- sprintf("http://www.cnyes.com/twstock/ps_historyprice/%s.htm", stock_id)
    res <- GET(url = url)
    tryCatch({
        # get data
        dat <- res %>% 
            content(as = "text", encoding = "UTF-8") %>% 
            `Encoding<-`("UTF-8") %>% 
            read_html() %>% 
            html_table(fill = TRUE) %>% 
            .[[2]]
    
    # type conversion 
    dat[, 2:ncol(dat)] <- sapply(dat[, 2:ncol(dat)], 
                                 function(x){
                                     x <- gsub(",|%", "", x)
                                     as.double(x)
                                 })
    dat$`日期` %<>% as.Date()
    
    return(dat)
    
    }, error = function(cond) {
        if (cond$call == ".[[2]]") {
            message("No data has been retrieved. Please check your stock id.")
        }
    }
)
}
