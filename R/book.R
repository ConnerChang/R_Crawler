library(rvest)
library(stringr)
library(tibble)

url <- "http://www.books.com.tw/web/sys_sublistb/books"
# get categories
categories <- url %>%
    read_html() %>%
    html_nodes(xpath = "//a") %>%
    html_attr(name = "href") %>%
    str_match("books/(.*)/") %>%
    .[, 2] %>%
    .[!is.na(.)] %>%
    .[-(1:3)]

# create an empty list to store books
book_list <- list(name = character(),
                  author = character(),
                  publisher = character(),
                  publication_date = character(),
                  descrip = character(),
                  item_url = character(),
                  category_1 = character(),
                  category_2 = character(),
                  price = character(),
                  discounted_price = character())

for (category in categories) {
    Sys.sleep(2)
    books_url <- 
        sprintf("http://www.books.com.tw/web/sys_bbotm/books/%s/?o=1&v=1&page=", category)
    
    category_1_ <- books_url %>% 
        read_html() %>% 
        html_nodes(xpath = "//li[@class='here']/span/a") %>% 
        html_text() %>% 
        paste0(., collapse = "_")
    
    category_2_ <- books_url %>% 
        read_html() %>% 
        html_nodes(xpath = "//li[@class='open']/span/a") %>% 
        html_text() 
    
    # get maximum page of each category
    max_page <- books_url %>% 
        read_html() %>% 
        html_nodes(xpath = "//div[@class='page']/span[1]") %>% 
        html_text() %>% 
        as.integer()
    
    if (length(max_page) == 0) max_page <- 1
    
    for (page in 1:max_page) {
        tryCatch({
        page_url <- paste0(books_url, page)
        
        html_doc <- page_url %>% 
            read_html()
        
        name_ <- html_doc %>% 
            html_nodes(xpath = "//h4/a") %>% 
            html_text()
        
        author_ <- html_doc %>%
            html_nodes(xpath = "//li[@class='info']/a") %>%
            html_text()
        
        publisher_ <- html_doc %>% 
            html_nodes(xpath = "//li[@class='info']/span") %>% 
            html_text() %>% 
            str_match("，(.*)，") %>% 
            .[, 2]
        
        publication_date_ <- html_doc %>% 
            html_nodes(xpath = "//li[@class='info']/span") %>% 
            html_text() %>% 
            str_match("出版日期：(.*)") %>% 
            .[, 2]
        
        descrip_ <- html_doc %>% 
            html_nodes(xpath = "//div[@class='txt_cont']/p") %>% 
            html_text() 
        
        item_url_ <- html_doc %>% 
            html_nodes(xpath = "//h4/a") %>% 
            html_attr(name = "href")
            
        price_ <- html_doc %>% 
            html_nodes(xpath = "//li[@class='set1']") %>% 
            html_text() %>% 
            str_match("定價：([0-9]*)") %>% 
            .[, 2]
        
        discounted_price_ <- html_doc %>% 
            html_nodes(xpath = "//li[@class='set2']") %>% 
            html_text() %>% 
            str_match("([0-9]*)元") %>% 
            .[, 2]
        
        if (length(author_) != length(name_)) {
            message(sprintf("Page %d in %s. Values of author are not complete.", page, category))
            next
        }
        
        # append data
        book_list$name <- c(book_list$name, name_)
        book_list$author <- c(book_list$author, author_)
        book_list$publisher <- c(book_list$publisher, publisher_)
        book_list$publication_date <- c(book_list$publication_date, publication_date_)
        book_list$descrip <- c(book_list$descrip, descrip_)
        book_list$item_url <- c(book_list$item_url, item_url_)
        book_list$category_1 <- c(book_list$category_1, rep(category_1_, length(name_)))
        book_list$category_2 <- c(book_list$category_2, rep(category_2_, length(name_)))
        book_list$price <- c(book_list$price, price_)
        book_list$discounted_price <- c(book_list$discounted_price, discounted_price_)
        
        message(sprintf("Page %d in %s. Number of rows: %d", page, category, length(book_list$name)))
        if (length(book_list$name) > 300000) break
        }, error = function(e) {
            message(sprintf("Error at Page %d in %s.", page, category))
        })
    }
}
# convert and clean data
book_dat <- as_tibble(book_list)
book_dat$price <- as.double(book_dat$price)
book_dat$discounted_price <- as.double(book_dat$discounted_price)
