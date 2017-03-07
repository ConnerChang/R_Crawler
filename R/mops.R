library(httr)
library(rvest)
library(XML)
library(magrittr)

get_mops_data <- function(type, code) {
    type_map <- list(
        "上市" = "sii",
        "上櫃" = "otc",
        "興櫃" = "rotc",
        "公開發行" = "pub"
    )
    code_map <- list(
        "水泥工業" = "01",
        "食品工業" = "02",
        "塑膠工業" =  "03",
        "紡織纖維" =  "04",
        "電機機械" =  "05",
        "電器電纜" =  "06",
        "化學工業" =  "21",
        "生技醫療業" =  "22",
        "化學生技醫療" =  "07",
        "玻璃陶瓷" =  "08",
        "造紙工業" =  "09",
        "鋼鐵工業" =  "10",
        "橡膠工業" =  "11",
        "汽車工業" =  "12",
        "半導體業" =  "24",
        "電腦及週邊設備業" =  "25",
        "光電業" =  "26",
        "通信網路業" =  "27",
        "電子零組件業" =  "28",
        "電子通路業" =  "29",
        "資訊服務業" =  "30",
        "其他電子業" =  "31",
        "電子工業" =  "13",
        "鋼鐵工業" =  "10",
        "油電燃氣業" =  "23",
        "建材營造" =  "14",
        "航運業" =  "15",
        "觀光事業" =  "16",
        "金融保險業" =  "17",
        "貿易百貨" =  "18",
        "綜合企業" =  "19",
        "其他" =  "20",
        "存託憑證" = "91",
        "文化創意業" = "32",
        "管理股票" =  "80",
        "鋼鐵工業" =  "10",
        "農業科技" = "33",
        "證金公司" = "97",
        "鋼鐵工業" =  "10",
        "期貨商" =  "98",
        "投信投顧公司" = "99",
        "證券" = "XX"
    )
    
    # set form data
    form <- list(encodeURIComponent = "1",
                 step = "1",
                 firstin = "1",
                 TYPEK = type_map[[type]],
                 code = code_map[[code]])
    
    # make a POST request
    url <- "http://mops.twse.com.tw/mops/web/ajax_t51sb01"
    res <- POST(url, 
                body = form, 
                encode = "form")
    
    # get and parse data
    doc_str <- res %>% 
        content(as = "text", encoding = "UTF-8")
    tryCatch({
        if (.Platform$OS.type == "unix") {
            dat <- doc_str %>%
                read_html(encoding = "UTF-8") %>%
                html_nodes(xpath = "//table[2]") %>%
                html_table(header = TRUE) %>% 
                .[[1]] # extract the tenth component of the html table
        } else if (.Platform$OS.type == "windows") {
            dat <- doc_str %>%
                read_html(encoding = "UTF-8") %>%
                html_nodes(xpath = "//table[2]") %>%
                as.character() %>%
                XML::readHTMLTable(encoding = "UTF-8") %>% 
                .[[1]] %>% # extract the tenth component of the html table
                `colnames<-`('公司代號','公司名稱','住址','營利事業統一編號',
                             '董事長','總經理','發言人','發言人職稱','代理發言人',
                             '總機電話','成立日期','上市日期','普通股每股面額',
                             '實收資本額(元)','已發行普通股數或TDR原發行股數',
                             '私募普通股(股)','特別股(股)','編製財務報告類型',
                             '股票過戶機構','過戶電話','過戶地址','簽證會計師事務所',
                             '簽證會計師1','簽證會計師2','英文簡稱','英文通訊地址',
                             '傳真機號碼','電子郵件信箱','公司網址',
                             '公司網站內利害關係人專區網址')
        } 
        return(dat)
    }, error = function(e) {
        message(sprintf("No data in type %s and code %s", type, code))
        return(data.frame())
    })
}