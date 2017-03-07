library(httr)
library(rvest)
library(magrittr)

get_starbucks_stores <- function(city) {
    city_map <- list(
        "台北市" = "11",
        "新北市" = "1",
        "桃園市" = "4",
        "新竹縣" = "2", 
        "新竹市" = "21",
        "苗栗縣" = "23",
        "台中市" = "7",
        "新竹市" = "4",
        "彰化縣" = "15",
        "雲林縣" = "3",
        "嘉義縣" = "12",
        "嘉義市" = "9",
        "台南市" = "26",
        "高雄市" = "24",
        "嘉義市" = "9",
        "屏東縣" = "13",
        "台東縣" = "16",
        "花蓮縣" = "20",
        "台東縣" = "16",
        "宜蘭縣" = "17",
        "南投縣" = "5",
        "宜蘭縣" = "17",
        "澎湖縣" = "19",
        "金門縣" = "25"
    )
    city_id <- city_map[[city]]
    
    url <- "http://www.starbucks.com.tw/stores/storesearch.jspx"
    res_g <- GET(url = url)
    
    view_state <- content(res_g) %>% 
        html_nodes("#javax\\.faces\\.ViewState") %>% # don't forget set escape character \\
        html_attr("value")
    
    form <- list(
        AJAXREQUEST = "sbForm:j_id_jsp_1422024916_2",
        name = "",
        road = "",
        selCity = city_id,
        selRegion = "ALL",
        selMetroSystem = "",
        selHighwayService = "",
        `sbForm:reserve` = "",
        `sbForm:drive` = "",
        `sbForm:fizzio` = "",
        `sbForm_SUBMIT` = "1",
        `javax.faces.ViewState` = view_state,
        `sbForm:j_id_jsp_1422024916_7` = "sbForm:j_id_jsp_1422024916_7"
    )
    
    res_p <- POST(url,
                  body = form,
                  encode = "form")
    
    stores <- content(res_p, as = "text") %>%
        `Encoding<-`("UTF-8") %>% 
        read_html() %>% 
        html_nodes(xpath = "//h3") %>% 
        html_text()
    
    stores_des <- content(res_p, as = "text") %>%
        `Encoding<-`("UTF-8") %>% 
        read_html() %>% 
        html_nodes(xpath = "//p") %>% 
        html_text() %>% 
        matrix(ncol = 3, byrow = TRUE)
    
    dat <- data.frame(
        store = stores,
        address =  stores_des[, 1],
        tel = stores_des[, 2],
        bussi_hour = stores_des[, 3],
        stringsAsFactors = FALSE
    )    
    return(dat)
}