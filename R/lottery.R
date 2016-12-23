library(httr)
library(rvest)
library(magrittr)
library(tibble)
# usage: get_lottery(103, 12)
get_lottery <- function(year, month) {
    url <- "http://www.taiwanlottery.com.tw/lotto/Lotto649/history.aspx"
    # get view state and event validation
    res_g <- GET(url = url)
    view_state <- content(res_g) %>% 
        html_nodes("#__VIEWSTATE") %>% 
        html_attr("value")
    event_validation <- content(res_g) %>% 
        html_nodes("#__EVENTVALIDATION") %>% 
        html_attr("value")
    
    form <- list(
        '__EVENTTARGET' = '',
        '__EVENTARGUMENT' =  '',
        '__LASTFOCUS' = '',
        '__VIEWSTATE' =  view_state,
        '__VIEWSTATEGENERATOR' = 'C3E8EA98',
        '__EVENTVALIDATION' =  event_validation,
        'Lotto649Control_history$DropDownList1' = '2',
        'Lotto649Control_history$chk' = 'radYM',
        'Lotto649Control_history$dropYear' =  year,
        'Lotto649Control_history$dropMonth' =  month,
        'Lotto649Control_history$btnSubmit' = '查詢')
    
    res_p <- POST(url = url, 
                body = form,
                encode = "form")
    
    doc <- content(res_p, as = "text", encoding = "UTF-8") %>% 
        `Encoding<-`("UTF-8") %>% 
        read_html() %>% 
        html_nodes(xpath = "//td/span") %>% 
        html_text() %>% 
        gsub(",|\r\n|\\s", "", .)
    
    dat <- matrix(doc, ncol = 40, byrow = TRUE) %>% 
        .[, -c(13:19)] %>% ## don't need the order of winning numbers
        as_tibble() %>% 
        set_colnames(c('期別', '開獎日', '兌獎截止', '銷售金額', '獎金總額', 
                       '獎號_1', '獎號_2', '獎號_3', '獎號_4', '獎號_5', '獎號_6',
                       '特別號' ,'頭獎_中獎注數', '貳獎_中獎注數', '參獎_中獎注數',
                       '肆獎_中獎注數', '伍獎_中獎注數', '陸獎_中獎注數', 
                       '柒獎_中獎注數', '普獎_中獎注數', '頭獎_單注獎金', 
                       '貳獎_單注獎金', '參獎_單注獎金', '肆獎_單注獎金', 
                       '伍獎_單注獎金', '陸獎_單注獎金', '柒獎_單注獎金', 
                       '普獎_單注獎金', '頭獎_累積至次期獎', '貳獎_累積至次期獎金',
                       '參獎_累積至次期獎金', '肆獎_累積至次期獎金', '伍獎_累積至次期獎金'))
    return(dat)
}
