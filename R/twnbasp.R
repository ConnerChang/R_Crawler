library(magrittr)
library(httr)
library(rvest)
library(jsonlite)
usrAgent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36"

options(HTTPUserAgent = usrAgent)
url <- "https://www.sportslottery.com.tw/web/services/rs/betting/tournamentGames/15102/0/t-4102.json?groups=278&groups=283&groups=289&groups=293&groups=683&locale=tw&brandId=defaultBrand&channelId=1"
httr::set_config(config(ssl_verifypeer = 0L))
res <- GET(url = url, user_agent(usrAgent))

dat <- content(res, as = "text") %>%
    `Encoding<-`("UTF-8") %>% 
    fromJSON()
