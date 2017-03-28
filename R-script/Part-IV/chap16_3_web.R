# http://media.daum.net/
# web crawling 시 고려사항 : 한글 지원 여부 

library(XML)
url <- "http://media.daum.net/"

# htmlTreeParse() : url의 문서를 html 태그 형식으로 파싱  
doc <- htmlTreeParse(url, useInternalNodes = T, trim = T, encoding = 'utf-8')

#xpathSApply(doc, "//태그명[@속성]", xmlValue)
news <- xpathSApply(doc, "//a[@class='link_txt']", xmlValue)
news

# 제어문자 제거 
# gsub() : "[]"안의 패턴을 찾아서 공백으로 교체 
result <- gsub("[\r\n\t]", "", news)
result <- gsub('[[:punct:]]',' ',result) # 문장부호 제거(\) 
result <- gsub('[[:cntrl:]]','',result) # 특수문자 제거
result <- gsub('\\d+','',result) # 숫자 제거 
result <- gsub('\\s+',' ',result) # 2개 이상 공백을 1개 공백으로 교체  


## 1. URL 생성
library(stringr)

urlbase <- "http://media.daum.net/newsbox/?tab_cate=NE&regDate="
date <- seq(20170201, 20170228, 1) 
date 
urllist <- str_c(urlbase, date) 
urllist 
page <- str_c('&page=',c(1:5)) ## 페이지 영역/ 날짜별로 Max 5페이지
page 

urllist_2 <- outer(urllist, page, str_c) ## (url + 날짜) + 페이지 
urllist_2

urllist_3 <- as.vector(urllist_2) ## Matrix -> Vector화 
urllist_3

urlfinal <- sort(urllist_3) ##날짜 순서로 Sort
urlfinal

## 2. Crawling Function 정의 ###
library(XML)

daumcraw <- function(url){
  doc <- htmlTreeParse(url, useInternalNodes = T, trim = T, encoding="utf-8")
  rootNode <- xmlRoot(doc)
  result <- xpathSApply(rootNode, "//ul[@class='list_etc2']", xmlValue)
  return(result)
}

## 3. for 문으로 Crawling 하기
idx <- 1 ## 초기화
result <- as.character()

for(url in urlfinal[1:length(urlfinal)]){   
  result[idx] <- daumcraw(url)  
  idx <- idx + 1
}

