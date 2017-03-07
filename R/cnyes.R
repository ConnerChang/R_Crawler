library(magrittr)
library(httr)
library(rvest)

get_stock_price <- function(stock_id) {
    Sys.sleep(1)
    url <- sprintf("http://www.cnyes.com/twstock/ps_historyprice/%s.htm", stock_id)
    tryCatch({
        # make a GET request
        res <- GET(url = url)
        # return an empty data frame if status_code >= 400
        if (res$status_code >= 400) {
            message(sprintf("Stock %s has error because of status code %i", stock_id, res$status_code))
            return(data.frame())
        }
        doc_str <- res %>% 
            content(as = "text", encoding = "UTF-8")
        
        if (.Platform$OS.type == "unix") { 
            # get and parse data
            dat <- doc_str %>% 
                read_html() %>% 
                html_table(fill = TRUE) %>% 
                .[[2]] # extract the second component of the html table
        } else if (.Platform$OS.type == "windows") { # html_table() doesn't work on Windows
            # get and parse data
            dat <- doc_str %>% 
                read_html(encoding = "UTF-8") %>%
                as.character() %>%
                XML::readHTMLTable(encoding = "UTF-8") %>% 
                .[[2]]
            
            # return an empty data frame if no data was retrieved
            if (is.null(dat)) {
                message(sprintf("Stock %s has no data.", stock_id))
                return(data.frame())
            }
            
            colnames(dat) <- c("日期", "開盤", "最高", "最低", "收盤",
                               "漲跌", "漲%", "成交量", "成交金額", "本益比")
        }
        # return a empty data frame if no data was retrieved
        if (nrow(dat) == 0) {
            message(sprintf("Stock %s has no data.", stock_id))
            return(data.frame())
        }
        
        # convert data type from character to double
        dat[, 2:ncol(dat)] <- sapply(dat[, 2:ncol(dat)], 
                                     function(x){
                                         x <- gsub(",|%", "", x)
                                         as.double(x)
                                     })
        # convert data type from character to date
        dat$`日期` %<>% as.Date()
        
        return(dat)
    }, error = function(cond) {
        # return an empty data frame if the error "subscript out of bounds" happens
        if (cond$call == ".[[2]]") { 
            message(sprintf("Stock %s has no data.", stock_id))
            return(data.frame())
        }
    })
}
