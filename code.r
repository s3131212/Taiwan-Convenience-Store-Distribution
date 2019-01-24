library(plyr)
library(dplyr)
library(Nippon)
library(stringr)
library(stringi)
library(jsonlite)
library(ggplot2)
library(ggmap)

register_google(key = "<Google API Key>")

# 中文字轉數字
chinese2digits <- function(x){
 vals <- sapply(str_split(x, "")[[1]], function(chi_digit){
   mapvalues(chi_digit, c("零", "一", "二", "三", "四", "五", "六", "七", "八", "九",
                          "十", "百", "千", "萬", "億"), c(0:10, 10^c(2,3,4,8)), FALSE)
 }) %>% as.integer
 digit_output <- 0
 base_term <- 1
 for (i in rev(seq_along(vals)))
 {
   if (vals[i] >= 10 && i == 1)
   {
     base_term <- ifelse(vals[i] > base_term, vals[i], base_term * vals[i])
     digit_output <- digit_output + vals[i]
   } else if (vals[i] >= 10)
   {
     base_term <- ifelse(vals[i] > base_term, vals[i], base_term * vals[i])
   } else
   {
     digit_output <- digit_output + base_term * vals[i]
   }
 }
 return(digit_output)
}

shop_data <- read.table("~/Desktop/R_Project/shop_data.csv", header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
shop_data <- shop_data %>%
    filter(分公司狀態 == "1") %>%
    select(公司名稱, 分公司統一編號, 分公司地址)

# 處理地址
# 不知道為什麼 mutate 弄不起來，只好用 for
for(i in 1:nrow(shop_data)){
  # 全形轉半形
  shop_data$分公司地址[i] <- zen2han(as.character(shop_data$分公司地址[i]))

  # 中文字轉數字
  # 程式碼修改自：https://www.ptt.cc/bbs/R_Language/M.1466006460.A.4DC.html
  shop_data$分公司地址[i] <- sapply(shop_data$分公司地址[i], function(x){
    pattern_starts <- "[零一二三四五六七八九十百千萬億]+樓"
    if (!str_detect(x, pattern_starts))
    return(x)
    stairs <- str_extract(x, pattern_starts)
    x <- str_replace(x, str_c("(\\d+)(", pattern_starts, ")"), "\\1, \\2")
    x <- str_replace(stairs, "樓", "") %>% chinese2digits %>% str_c("樓") %>%
    {str_replace(x, stairs, .)}
    return(x)
  }) %>% `names<-`(NULL)

  shop_data$分公司地址[i] <- sapply(shop_data$分公司地址[i], function(x){
    pattern_starts <- "[零一二三四五六七八九十百千萬億]+F"
    if (!str_detect(x, pattern_starts))
    return(x)
    stairs <- str_extract(x, pattern_starts)
    x <- str_replace(x, str_c("(\\d+)(", pattern_starts, ")"), "\\1, \\2")
    x <- str_replace(stairs, "F", "") %>% chinese2digits %>% str_c("F") %>%
    {str_replace(x, stairs, .)}
    return(x)
  }) %>% `names<-`(NULL)

  shop_data$分公司地址[i] <- sapply(shop_data$分公司地址[i], function(x){
    pattern_starts <- "[零一二三四五六七八九十百千萬億]+號"
    if (!str_detect(x, pattern_starts))
    return(x)
    no <- str_extract(x, pattern_starts)
    x <- str_replace(x, str_c("(\\d+)(", pattern_starts, ")"), "\\1, \\2")
    x <- str_replace(no, "號", "") %>% chinese2digits %>% str_c("號") %>%
    {str_replace(x, no, .)}
    return(x)
  }) %>% `names<-`(NULL)
}

# 異體字
shop_data <- shop_data %>% mutate(分公司地址 = {
  分公司地址 %>%
  str_replace_all("台", "臺") %>%
  str_replace_all("巿", "市")
})

# 縣轄市應算在縣底下
shop_data <- shop_data %>% mutate(分公司地址 = {
  分公司地址 %>%
  str_replace_all("^竹北市", "新竹縣竹北市") %>%
  str_replace_all("^彰化市", "彰化縣彰化市") %>%
  str_replace_all("^員林市", "彰化縣員林市") %>%
  str_replace_all("^苗栗市", "苗栗縣苗栗市") %>%
  str_replace_all("^頭份市", "苗栗縣頭份市") %>%
  str_replace_all("^南投市", "南投縣南投市") %>%
  str_replace_all("^斗六市", "雲林縣斗六市") %>%
  str_replace_all("^太保市", "嘉義縣太保市") %>%
  str_replace_all("^朴子市", "嘉義縣朴子市") %>%
  str_replace_all("^屏東市", "屏東縣屏東市") %>%
  str_replace_all("^宜蘭市", "宜蘭縣宜蘭市") %>%
  str_replace_all("^花蓮市", "花蓮縣花蓮市") %>%
  str_replace_all("^臺東市", "臺東縣臺東市") %>%
  str_replace_all("^馬公市", "澎湖縣馬公市")
})
# 處理直轄市改制後的名稱
shop_data <- shop_data %>% mutate(分公司地址 = {
  分公司地址 %>%
  str_replace_all("臺北縣", "新北市") %>%
  str_replace_all("桃園縣", "桃園市") %>%
  str_replace_all("臺中縣", "臺中市") %>%
  str_replace_all("臺南縣", "臺南市") %>%
  str_replace_all("高雄縣", "高雄市")
})

shop_data <- shop_data %>% mutate(city = substring(分公司地址, 0, 3))
shop_data <- shop_data %>% mutate_geocode(分公司地址)

# 計算超商數量
shop_count <- shop_data %>%
    group_by(公司名稱) %>%
    dplyr::summarise(n = n_distinct(分公司統一編號))
ggplot(data=shop_count, mapping=aes(x="公司名稱", y = n ,fill=公司名稱)) +
    geom_bar(stat="identity",width = 1, size = 1,position='stack') +
    coord_polar("y", start=0) +
    theme_void() +
    labs(fill='超商') +
    geom_text(aes( label = scales::percent(n / sum(n))), position = position_stack(vjust = 0.5)) +
    theme(text=element_text(family="黑體-繁 中黑", size=12))

town_list <- fromJSON("~/Desktop/R_Project/town_list.json")
population <- fromJSON("~/Desktop/R_Project/population.json")
area <- fromJSON("~/Desktop/R_Project/area.json")

# 每個縣市的總店數
shop_per_city <- shop_data %>% group_by(city, 公司名稱) %>% dplyr::summarise(n = n_distinct(分公司統一編號))
ggplot(data = shop_per_city, mapping = aes(x = city, y = n, fill = shop_per_city$公司名稱)) +
    geom_bar(stat = "identity") +
    labs(x = '縣市', y = '數量', fill='超商') +
    theme(text=element_text(family="黑體-繁 中黑", size=12), axis.text.x=element_text(angle=45))

# 平均每家店服務的人口數
. <- shop_per_city %>%
    group_by(city) %>%
    summarise(num = sum(n))
people_per_shop <- left_join(., population, by = c("city" = "city"))
ggplot(data = people_per_shop, mapping = aes(x = city, y = (population / num) / 1e2, fill = people_per_shop$city)) +
    geom_bar(stat = "identity") +
    labs(x = '縣市', y = '服務人口（百人）', fill='縣市') +
    geom_text(aes(label = floor((population / num) / 10) / 10), size = 3,  position = position_stack(vjust = 0.5)) +
    theme(text=element_text(family="黑體-繁 中黑", size=12), axis.text.x=element_text(angle=45), legend.position="none")

# 平均每家店服務的範圍
area_per_shop <- left_join(., area, by = c("city" = "city"))
ggplot(data = area_per_shop, mapping = aes(x = city, y = (area / num), fill = area_per_shop$city)) +
    geom_bar(stat = "identity") +
    labs(x = '縣市', y = '服務範圍（平方公里）', fill='縣市') +
    geom_text(aes(label = floor((area / num) * 100) / 100), size = 3,  position = position_stack(vjust = 0.5)) +
    theme(text=element_text(family="黑體-繁 中黑", size=12), axis.text.x=element_text(angle=45), legend.position="none")



# 繪製地圖
qmplot(lon, lat, data = shop_data, maptype = "toner-lite", color = I("red"), zoom = 9)

# 臺北: 25.108215, 121.451381   24.989190, 121.570576
travel_time_taipei <- c()
for(i in 1:200){
    lon <- runif(1, min=121.451381, max=121.570576)
    lat <- runif(1, min=24.989190, max=25.108215)
    dis <- (shop_data$lon - lon) ^ 2 + (shop_data$lat - lat) ^ 2
    . <- mapdist(as.numeric(c(lon, lat)), c(shop_data$分公司地址[which.min(dis)]), mode="walking")
    if(is.numeric(.$seconds)) travel_time_taipei <- c(travel_time_taipei, .$seconds)
}


# 雲林嘉義 23.824379, 120.251754   23.405572, 120.486646
travel_time_yunlin_chiayi <- c()
for(i in 1:200){
    lon <- runif(1, min=121.451381, max=121.570576)
    lat <- runif(1, min=24.989190, max=25.108215)
    dis <- (shop_data$lon - lon) ^ 2 + (shop_data$lat - lat) ^ 2
    . <- mapdist(as.numeric(c(lon, lat)), c(shop_data$分公司地址[which.min(dis)]), mode="walking")
    if(is.numeric(.$seconds)) travel_time_yunlin_chiayi <- c(travel_time_yunlin_chiayi, .$seconds)
}

# 宜蘭 24.614054, 121.805200  24.786233, 121.654558
travel_time_yilan <- c()
for(i in 1:200){
    lon <- runif(1, min=121.451381, max=121.570576)
    lat <- runif(1, min=24.989190, max=25.108215)
    dis <- (shop_data$lon - lon) ^ 2 + (shop_data$lat - lat) ^ 2
    . <- mapdist(as.numeric(c(lon, lat)), c(shop_data$分公司地址[which.min(dis)]), mode="walking")
    if(is.numeric(.$seconds)) travel_time_yilan <- c(travel_time_yilan, .$seconds)
}
