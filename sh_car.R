rm(list = ls())
library(httr)
headers = c(Connection = "keep-alive",
            `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.98 Safari/537.36")
cookies = c(antipas = "2P1t973w8N6969731gd33T",
            uuid = "db2e6b56-1dbc-44f0-e5d9-5ba04d7d6919",
            cityDomain = "sh",
            ganji_uuid = "7946086973180744420356",
            sessionid = "7cf95943-7ccd-4491-9605-ae5f5efcaf32")

# 汽车页面链接
library(xml2)
library(jsonlite)
pinpai <- c("dazhong","buick","byd","bmw","benz","rongwei","audi","ford","chevrolet","skoda","toyota","honda","kia","hyundai")
brand_chexing <- expand.grid(pinpai, 2:8, stringsAsFactors = F)
carlist_page1 <- character(14*7)  # 前14个品牌&7个车型第1页
carlist_page2 <- character(14*7)  # 前14个品牌&7个车型第2页

for (k in 1:nrow(brand_chexing)) {
  carlist_page1[k] <- paste0("https://www.guazi.com/sh/", brand_chexing[k,1], "/o1h", brand_chexing[k,2], "/#bread")  # 每个品牌-车型的第1页
  carlist_page2[k] <- paste0("https://www.guazi.com/sh/", brand_chexing[k,1], "/o2h", brand_chexing[k,2], "/#bread")  # 第2页
}

# 判断是否有结果&取汽车链接
all_car_df <- data.frame()  # 储存所有汽车的品牌-车型-汽车链接
for (kk in 1:nrow(brand_chexing)) {  # 每个指定品牌&车型
  list_page <- GET(carlist_page1[kk],  # 抓取第一页页面
                   add_headers(headers), set_cookies(cookies))
  list_dom <- content(list_page)
  index_nodes <- xml_find_all(list_dom, './/div[@class="list-wrap js-post"]/input')
  index <- fromJSON(xml_attr(index_nodes, "value"))$num  # 判断制定品牌&车型的汽车数量
  chexing <- fromJSON(xml_attr(index_nodes, "value"))$chexing  # 车型
  pinpai <- fromJSON(xml_attr(index_nodes, "value"))$brand # 品牌
  if (index == 0) {
    print(paste0(pinpai,"品牌", chexing, "车型没有车啦"))
  } else if (index <= 40) {
    print("只有<=40辆车哦，不用翻页")
    page_nodes <- xml_find_all(list_dom, './/div[@class="list-wrap js-post"]/ul[@class="carlist clearfix js-top"]/li/a')
    car_url <- paste0("https://www.guazi.com", xml_attr(page_nodes, "href"))
    brand_chexing_carurl <- data.frame(pinpai = rep(pinpai, length(car_url)),
                                       chexing = rep(chexing, length(car_url)),
                                       car_url = car_url, stringsAsFactors = F)
    print(paste0("抓取了", nrow(brand_chexing_carurl), "条链接"))
  } else if (index <= 50) {
    print("大于40辆小于50辆车，翻页")
    # 第一页
    page_nodes <- xml_find_all(list_dom, './/div[@class="list-wrap js-post"]/ul[@class="carlist clearfix js-top"]/li/a')
    car_url <- paste0("https://www.guazi.com", xml_attr(page_nodes, "href"))
    brand_chexing_carurl <- data.frame(pinpai = rep(pinpai, length(car_url)),
                                       chexing = rep(chexing, length(car_url)),
                                       car_url = car_url, stringsAsFactors = F)
    # 第二页
    list_page2 <- GET(carlist_page2[kk],  # 获取第二页页面
                     add_headers(headers), set_cookies(cookies))
    list_dom2 <- content(list_page2)
    page_nodes2 <- xml_find_all(list_dom2, './/div[@class="list-wrap js-post"]/ul[@class="carlist clearfix js-top"]/li/a')
    car_url2 <- paste0("https://www.guazi.com", xml_attr(page_nodes2, "href"))
    add_df <- data.frame(pinpai = rep(pinpai, length(car_url2)),
                        chexing = rep(chexing, length(car_url2)),
                        car_url = car_url2, stringsAsFactors = F)
    brand_chexing_carurl <- rbind(brand_chexing_carurl, add_df)
    print(paste0("抓取了", nrow(brand_chexing_carurl), "条链接"))
    
  } else {
    print("大于50辆车哈哈")
    # 第一页
    page_nodes <- xml_find_all(list_dom, './/div[@class="list-wrap js-post"]/ul[@class="carlist clearfix js-top"]/li/a')
    car_url <- paste0("https://www.guazi.com", xml_attr(page_nodes, "href"))
    brand_chexing_carurl <- data.frame(pinpai = rep(pinpai, length(car_url)),
                                       chexing = rep(chexing, length(car_url)),
                                       car_url = car_url, stringsAsFactors = F)
    # 第二页
    list_page2 <- GET(carlist_page2[kk],
                      add_headers(headers), set_cookies(cookies))
    list_dom2 <- content(list_page2)
    page_nodes2 <- xml_find_all(list_dom2, './/div[@class="list-wrap js-post"]/ul[@class="carlist clearfix js-top"]/li/a')
    car_url2 <- paste0("https://www.guazi.com", xml_attr(page_nodes2, "href"))[1:10]  # 第二页只取10个
    add_df <- data.frame(pinpai = rep(pinpai, length(car_url2)),
                         chexing = rep(chexing, length(car_url2)),
                         car_url = car_url2, stringsAsFactors = F)
    brand_chexing_carurl <- rbind(brand_chexing_carurl, add_df)
    print(paste0("抓取了", nrow(brand_chexing_carurl), "条链接"))
  }
  all_car_df <- rbind(all_car_df, brand_chexing_carurl)
  brand_chexing_carurl <- NULL
  add_df <- NULL
  if (kk %% 10 == 0) {
    Sys.sleep(60)
  }
}

id <- data.frame(id = 1:nrow(all_car_df))
all_car_df <- cbind(id, all_car_df)
all_car_df$car_url <- as.character(all_car_df$car_url)

write.csv(all_car_df, "E:\\renyimeng\\taskfolder\\mycardata\\id_pinpai_cehxing_url.csv", row.names = F)

# 爬取图片链接
shangpai <- character(nrow(all_car_df))
licheng <- character(nrow(all_car_df))
pailiang <- numeric(nrow(all_car_df))
biansu <- character(nrow(all_car_df))
baojia <- character(nrow(all_car_df))
yuanjia <- character(nrow(all_car_df))
for (i in 1391:nrow(all_car_df)){
  # 每个汽车页面存出6个图片
  d = GET(all_car_df$car_url[i],
          add_headers(headers), set_cookies(cookies))
  dom <- content(d)
  img_nodes <- xml_find_all(dom, './/ul[@class="clearfix"]/li/img/@src')
  img_url <- gsub("@base@tag=imgScale&w=120&h=80&c=1&m=2&q=88", "", xml_text(img_nodes)[1:6])
  img_url <- gsub("?imageView2/1/w/120/h/80/q/88", "", img_url)
  for (j in 1:6){
    GET(img_url[j], write_disk(paste0("E:\\renyimeng\\taskfolder\\ershouche2\\",i,"-",j,".jpg"), overwrite=TRUE))
  }
  print(paste0("第", i, "辆车共", j, "张图片已保存"))
  info_nodes <- xml_text(xml_find_all(dom, './/ul[@class="assort clearfix"]/li/span'))
  shangpai[i] <- info_nodes[1]
  licheng[i] <- info_nodes[2]
  pailiang[i] <- as.numeric(info_nodes[4])
  biansu[i] <- info_nodes[5]
  baojia[i] <- xml_text(xml_find_all(dom, './/div[@class="pricebox js-disprice"]/span[@class="pricestype"]'))
  yuanjia[i] <- xml_text(xml_find_all(dom, './/div[@class="pricebox js-disprice"]/span[@class="newcarprice"]'))
  print(paste0("第", i, "信息已提取"))
  if (i %% 20 == 0) {
    Sys.sleep(60)
  }
}

# 处理数据格式
licheng <- as.numeric(gsub("万公里", "", licheng))
temp <- gsub("                万", "", baojia)
temp <- gsub("补贴后","\\T", temp)
baojia<- as.numeric(substr(temp, 2, nchar(temp)))
temp2 <- gsub("\r\n                            新车指导价", "", yuanjia)
temp3 <- gsub("\r\n                    ", "", temp2)
temp4 <- gsub("\r\n            ", "", temp3)
temp5 <- gsub("\r\n                ", "", temp4)
yuanjia <- as.numeric(gsub("万\\(含税\\)|万|原价", "", temp5))  # 原价有29个空值

all_car_df$licheng <- licheng  # 里程
all_car_df$shangpai <- shangpai  # 上牌时间
all_car_df$pailiang <- pailiang  # 排量，有9个空值
all_car_df$biansu <- biansu  # 变速
all_car_df$baojia <- baojia  # 报价
all_car_df$yuanjia <- yuanjia  # 原价，有29个空值

GetChexing <- function(str) {
  cx <- ifelse(str == "2", "SUV", 
               ifelse(str == "3", "MPV", 
                      ifelse(str == "4", "跑车", 
                             ifelse(str == "5", "两厢轿车", 
                                    ifelse(str == "6", "三厢轿车", 
                                           ifelse(str == "7", "面包车", "皮卡"))))))
  return(cx)
}

all_car_df$chexing <- GetChexing(all_car_df$chexing)
write.csv(all_car_df, "E:\\renyimeng\\taskfolder\\mycardata\\car_new.csv", row.names = F)
