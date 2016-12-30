library(magrittr)
library(httr)
library(rvest)
# data format yyyy/mm/dd
get_stock_price <- function(stock_id, start_date, end_date) {
    tryCatch({
        url <- sprintf("http://www.cnyes.com/twstock/ps_historyprice/%s.htm", stock_id)
        res_g <- GET(url = url)
        
        view_state <- content(res_g) %>% 
            html_nodes("#__VIEWSTATE") %>% 
            html_attr("value") %>% 
            .[1]
        
        view_state_generator <- content(res_g) %>% 
            html_nodes("#__VIEWSTATEGENERATOR") %>% 
            html_attr("value")
        
        event_validation <- content(res_g) %>% 
            html_nodes("#__EVENTVALIDATION") %>% 
            html_attr("value")
        
        form <- list(
            `__VIEWSTATE` = view_state,
            `__VIEWSTATEGENERATOR` = view_state_generator,
            `__EVENTVALIDATION` = event_validation,
            pageTypeHidden = "1",
            code = "2330",
            `ctl00$ContentPlaceHolder1$startText` = start_date, 
            `ctl00$ContentPlaceHolder1$endText` = end_date,
            `ctl00$ContentPlaceHolder1$submitBut` = "查詢"
        )
        
        res_p <- POST(url,
                      body = form,
                      encode = "form")
        
        dat <- res_p %>% 
            content(as = "text", encoding = "UTF-8") %>% 
            `Encoding<-`("UTF-8") %>% 
            read_html() %>% 
            html_table(fill = TRUE) %>% 
            .[[2]]
        
        if (nrow(dat) != 0) {
            stop("No data has been retrieved. Please check the function arguments.")
        }
        
        dat[, 2:ncol(dat)] <- sapply(dat[, 2:ncol(dat)], 
                                     function(x){
                                         x <- gsub(",|%", "", x)
                                         as.double(x)
                                     })
        dat$`日期` %<>% as.Date()
        
        return(dat)
    }, error = function(cond) {
        if (cond$call == ".[[2]]") {
            message("No data has been retrieved. Please check the function arguments.")
        }
    })
}
