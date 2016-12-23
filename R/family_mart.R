library(magrittr)
library(httr)
library(jsonlite)

res <- GET("http://api.map.com.tw/net/familyShop.aspx",
           add_headers(
               Referer = "http://www.family.com.tw/marketing/inquiry.aspx"
           ),
           query = list(
               searchType = "ShopList",
               type = "",
               city = "基隆市",
               area = "仁愛區",
               road = "",
               fun = "showStoreList",
               key = "6F30E8BF706D653965BDE302661D1241F8BE9EBC"
           ))

resStr <- content(res,as = "text") %>% 
    `Encoding<-`("UTF-8")


jsonDataString <- resStr %>%
    sub("^[^\\[]*", "", .) %>%
    sub("[^\\]]*$", "", .)

dt <- fromJSON(jsonDataString)
