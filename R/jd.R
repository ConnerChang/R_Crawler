library(rvest)
library(jsonlite)
library(stringr)
library(tibble)
library(RCurl)

create_category_df <- function() {
    category_df <- tibble(
        category_1 = character(0),
        category_2 = character(0),
        category_3 = character(0),
        category_url = character(0)
    )
    
    all_cat_url <- "http://www.jd.com/allSort.aspx"
    cat_doc <- all_cat_url %>% 
        read_html()
    
    category_1 <- cat_doc %>% 
        html_nodes(xpath = "//h2[@class='item-title']/span") %>% 
        html_text()
    
    for (i in 1:11) {
        for (j in 1:2) {
            category_2 <- cat_doc %>% 
                html_nodes(xpath = 
                               sprintf("//div[@class='col'][%d]/div[@class='category-item m'][%d]/div[@class='mc']/div[@class='items']/dl[@class='clearfix']/dt/a", j, i)) %>% 
                html_text()
            
            df_category_2 <- tibble(
                category_2 = character(0),
                category_3 = character(0),
                category_url = character(0)
            )
            for (k in seq_along(category_2)) {
                category_3_ <- cat_doc %>% 
                    html_nodes(xpath = 
                                   sprintf("//div[@class='col'][%d]/div[@class='category-item m'][%d]/div[@class='mc']/div[@class='items']/dl[@class='clearfix'][%d]/dd/a", j, i, k)) %>% 
                    html_text()
                
                category_url_ <- cat_doc %>% 
                    html_nodes(xpath = 
                                   sprintf("//div[@class='col'][%d]/div[@class='category-item m'][%d]/div[@class='mc']/div[@class='items']/dl[@class='clearfix'][%d]/dd/a", j, i, k)) %>% 
                    html_attr(name = "href")
                
                df_category_3 <- tibble(
                    category_2 = category_2[k],
                    category_3 = category_3_,
                    category_url = category_url_
                )
                df_category_2 <- rbind(
                    df_category_2,
                    df_category_3
                )
            }
            # if j = 2, meaning that we are now at index i + (j * 11 - 11) of category_1
            cat1_index <- i + (j * 11 - 11)
            if (!is.na(category_1[cat1_index])) { # there's no category_1[22]
                df_category_2$category_1 <- category_1[cat1_index]
            }
            category_df <- rbind(category_df, df_category_2)
        }
    }
    category_df$category_url <- paste0("http:", category_df$category_url)
    category_df <- category_df[grepl(category_df$category_url, pattern = "cat="), ]
    return(category_df)
}

category_df <- create_category_df()
category_df <- dplyr::filter(category_df, category_1 != "图书、音像、电子书刊")
item_list = list(
        item = character(),
        item_id = character(),
        item_url = character(),
        category_1 = character(),
        category_2 = character(),
        category_3 = character()
)

for (row in 1:nrow(category_df)) {
    url <- category_df$category_url[row]
    html_doc <- url %>%
        read_html() 
    
    max_page <- html_doc %>% 
        html_nodes(xpath = "//em/b") %>% 
        html_text() %>% 
        as.integer()
    
    if (length(max_page) == 0) next
    
    # category_count <- 0
    for (page in 1:5) {
        category_url <- sprintf("%s&page=%d", url, page)    
        
        category_html_doc <- category_url %>% 
            read_html()
        
        item_ <- category_html_doc %>% 
            html_nodes(xpath = "//div[@class='p-name']/a/em") %>% 
            html_text()
        
        item_id_ <- category_html_doc %>% 
            html_nodes(xpath = "//a[@class='p-o-btn focus J_focus']") %>% 
            html_attr(name = "data-sku")
        
        item_url_ <- category_html_doc %>% 
            html_nodes(xpath = "//div[@class = 'p-img']/a") %>% 
            html_attr(name = "href")
        
        if (!((length(item_) == length(item_id_)) & (length(item_id_) == length(item_url_)))) {
            message("The Lengths is not equal.")
            next
        }
        
        item_list$item <- c(item_list$item, item_)
        item_list$item_id <- c(item_list$item_id, item_id_)
        item_list$item_url <- c(item_list$item_url, item_url_)
        item_list$category_1 <- c(item_list$category_1, rep(category_df$category_1[row], length(item_)))
        item_list$category_2 <- c(item_list$category_2, rep(category_df$category_2[row], length(item_)))
        item_list$category_3 <- c(item_list$category_3, rep(category_df$category_3[row], length(item_)))
        
        message(sprintf("Get items at page %d of category %s. Number of rows: %d.", page, category_df$category_3[row], effec_rows))
    }
}

item <- as_tibble(item_list)
nrow_item <- nrow(item)

price_vector <- paste0("J_", item$item_id) 
price_dt <- data.table(
    id = character(),
    p = character()
)
for (i in 1:(nrow_item %/% 61 + 1)) {
    tryCatch({
        price_ <- paste0(price_vector[(1 + 61 * (i - 1)):(61 + 61 * (i - 1))], collapse = "%2C") %>% 
            paste0("http://p.3.cn/prices/mgets?callback=jQuery9430244&skuIds=", ., "&pduid=14852575521831760243282") %>% 
            RCurl::getURL() %>% 
            gsub("^[^\\[]*", "", .) %>%
            gsub("[);\n]", "", .) %>% 
            fromJSON() %>% 
            .[, 1:2]
        
        price_dt <- rbind(price_dt, price_)
        message(i, " ", nrow(price_dt))
    }, error = function(e){
        message(i)
    }
    )
}
price_dt[,id := substring(id, 3)]
item <- merge(item, price_dt, 
              by.x = "item_id", by.y = "id", all = TRUE) %>% 
             {
                .[!duplicated(.)]
             }

ad_vector <- paste0("AD_", item$item_id) 
ad_dt <- data.table(
    id = character(),
    ad = character()
)

for (i in 1:(nrow_item %/% 61 + 1)) {
    tryCatch({
    ad_ <- 
        paste0(ad_vector[(1 + 61 * (i - 1)):(61 + 61 * (i - 1))], collapse = "%2C") %>% 
        paste0("http://ad.3.cn/ads/mgets?&callback=jQuery9793161&skuids=", .) %>% 
        RCurl::getURL() %>%
        gsub("^[^\\[]*", "", .) %>%
        gsub("[);\n]", "", .) %>% 
        str_split(",", simplify = TRUE) %>% 
        gsub("<a.*</a>", "", .) %>% 
        paste0(., collapse = ",") %>% 
        fromJSON() %>% 
        as.data.table()
    
    ad_dt <- rbind(ad_dt, ad_)
    message(i, " ", nrow(ad_dt))
    }, error = function(e) {
        message(i)
    })
}
ad_dt[,id := substring(id, 4)]
item <- 
    merge(item, ad_dt, by.x = "item_id",
          by.y = "id", all.x = TRUE) %>% 
          {
              .[!duplicated(.)]
          }