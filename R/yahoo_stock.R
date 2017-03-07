library(magrittr)
library(httr)
library(rvest)
library(XML)

get_yahoo_stock <- function(stock_id) {
    # get the html page that stores view state
    url <- sprintf("https://tw.stock.yahoo.com/d/s/major_%i.html", stock_id)
    # make a GET request
    res <- GET(url = url)
    # error detect by status code
    if (res[["status_code"]] >= 400) {
        # error handle
        message(sprintf("Stock %s has error of %i", stock_id, res[["status_code"]]))
        return(data.frame())
    }
    # get and parse data
    doc_str <- res %>% 
        content(as = "text", encoding = "big-5")
    tryCatch({
        if (.Platform$OS.type == "unix") {
            dat <- doc_str %>% 
                read_html() %>% 
                html_table(header = TRUE, fill = TRUE) %>% 
                .[[10]] # extract the tenth component of the html table
        } else if (.Platform$OS.type == "windows") {
            dat <- doc_str %>%
                read_html(encoding = "UTF-8") %>%
                as.character() %>%
                XML::readHTMLTable(header = TRUE, encoding = "UTF-8") %>%
                .[[10]] # extract the tenth component of the html table
        } 
        # terminate program if no data has been retrieved
        if (nrow(dat) == 0) {
            message(sprintf("Stock %s has no data.", stock_id))
            return(data.frame())
        }
        # reset variables of each column
        colnames(dat) <- c("券商", "買進", "賣出", "買賣超",
                           "券商", "買進", "賣出", "買賣超")
        
        dat <- rbind(dat[, 1:4], dat[, 5:8])
        # convert data type from character to double
        dat[, c(2:4)] <- sapply(dat[, c(2:4)],
                                function(x) {
                                    as.integer(x)
                                })
        return(dat)
    }, error = function(cond){
        # terminate program if the error "subscript out of bounds" happens (e.g., set stock_id = "0")
        if (cond$call == ".[[10]]") { 
            message(sprintf("Stock %s has no data.", stock_id))
            return(data.frame())
        }
    })
}