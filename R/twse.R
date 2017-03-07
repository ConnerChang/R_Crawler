library(httr)
library(rvest)
library(magrittr)
library(XML)
library(stringr)
# usage: get_twse(105, 12, 21)

get_twse <- function(year, month, day) {
    url <- "http://www.twse.com.tw/ch/trading/exchange/BWIBBU/BWIBBU_d.php"
    
    # month and day are must of two digits, e.g., "2" -> "02"
    month <- stringr::str_pad(month, 2, pad = "0")
    day <- stringr::str_pad(day, 2, pad = "0")
    
    tryCatch({
        # set form data
        form <- list(input_date = paste(year, month, day, sep = "/"),
                     select2 = "ALL",
                     order = "STKNO",
                     login_btn = "%ACd%B8%DF")
        # make a POST request
        res_p <- POST(url = url,
                      body = form,
                      encode = "form")
        # get and parse data
        if (.Platform$OS.type == "unix") {
            dat <- res_p %>% 
                content(encoding = 'big-5') %>%
                html_table(fill = TRUE) %>% 
                .[[8]] %>% 
                .[-1, ] %>% # remove first row 
                `colnames<-`(c('id','company','PER','DY','PBR'))
        } else if (.Platform$OS.type == "windows") {# html_table() doesn't work on Windows
            dat <- res_p %>% 
                content(as = "text", encoding = 'big-5') %>%
                read_html(encoding = "UTF-8") %>%
                as.character() %>% 
                XML::readHTMLTable(header = TRUE, encoding = "UTF-8") %>% 
                .[[8]] %>% 
                `colnames<-`(c('id','company','PER','DY','PBR'))
        }
        # convert data type from character to double
        dat[, 3:ncol(dat)] <- sapply(dat[, 3:ncol(dat)], 
                                     function(x){
                                         x <- gsub("-", "0", x)
                                         as.double(x)
                                     })
        return(dat)
    }, error = function(cond) {
        #  return an empty data frame if the error "subscript out of bounds" happens
        if (cond$call == ".[[8]]") { 
            message(sprintf("No data on %s/%s/%s", year, month, day))
            return(data.frame())
        }
    })
}