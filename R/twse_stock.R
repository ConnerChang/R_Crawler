suppressPackageStartupMessages({
    library(httr)
    library(rvest)
    library(magrittr)
    library(XML)
})
# http://www.tse.com.tw/ch/trading/exchange/STOCK_DAY/STOCK_DAYMAIN.php
get_stock_info <- function(year, month, stock_id) {
    url <- "http://www.tse.com.tw/ch/trading/exchange/STOCK_DAY/STOCK_DAYMAIN.php"
    
    form <- list(download = "",
                 query_year = year,
                 query_month = month,
                 CO_ID = stock_id,
                 `query-button` = "%E6%9F%A5%E8%A9%A2")
    
    res <- POST(url = url, body = form, encode = "form")
    if (.Platform$OS.type == "unix") { 
        dat <- res %>% 
            content(encoding = 'UTF-8') %>%
            html_table(fill = TRUE, header = TRUE) %>% 
            .[[1]] %>% 
            `colnames<-`(.[1, ]) %>% 
            .[2:nrow(.), ]
    } else {   
        dat <- res %>% 
            content(as = "text", encoding = 'UTF-8') %>%
            read_html(encoding = "UTF-8") %>%
            as.character() %>% 
            XML::readHTMLTable(header = TRUE, encoding = "UTF-8") %>% 
            .[[1]] %>% 
            `colnames<-`(c('日期','成交股數','成交金額','開盤價','最高價','最低價','收盤價',
                      '漲跌價差','成交筆數'))
    }
    dat[, 2:ncol(dat)] <- sapply(dat[, 2:ncol(dat)],
                                 function(x){
                                        x <- gsub(",", "", x)
                                        as.double(x)
                                        })
    return(dat)
}

