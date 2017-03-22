# Formal(2_Maria_DB)

# Formal(2_MariaDB)
# Maria DB 정형 데이터 처리

# 패키지 설치
# - RJDBC 패키지를 사용하기 위해서는 우선 java를 설치해야 한다.
#install.packages("rJava")
#install.packages("DBI")
#$install.packages("RJDBC") # JDBC()함수 제공 

# 패키지 로딩
library(DBI)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
library(rJava)
library(RJDBC) # rJava에 의존적이다.

################ MariaDB or MySql ###############
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="C:\\NCS\\python\\util\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40\\mysql-connector-java-5.1.40-bin.jar")

# driver가 완전히 로드된 후 db를 연결한다.
conn <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/work", "scott", "tiger")
#################################################           

# DB 연결 확인 :  테이블의 컬럼 보기 
dbListFields(conn, "goods") # conn, table name
# [1] "code" "name" "su"   "dan" 

query <- "select * from goods"
dbGetQuery(conn, query)

query <- "select * from goods where dan >= 700000"
dbGetQuery(conn, query) # 2개 레코드 

# db 구조 변경 
# 1. 레코드 삽입 
dbSendUpdate(conn, "insert into goods values(6, '전화기', 4, 450000)")
dbGetQuery(conn, "select * from goods")

# 2. 레코드 수정
dbSendUpdate(conn, "update goods set name = 'phone' where code=6")
dbGetQuery(conn, "select * from goods")

# 3. 레코드 삭제 
dbSendUpdate(conn, "delete from goods where code=6")
dbGetQuery(conn, "select * from goods")

# file -> DB 저장 
recode <- read.csv("C:\\NCS\\Rwork\\data\\recode.csv", header = T)
recode
dbWriteTable(conn, 'goods', recode) # 기존 'goods' 데이터는 손실 

goods <- dbGetQuery(conn, 'select * from goods')
str(goods) # 'data.frame':	5 obs. of  4 variables:
goods

#price = [] # list 
price <- numeric() # vector

for(i in 1:nrow(goods)){ # 1 ~ 5
  price[i] <- goods$su[i] * goods$dan[i]
}
price

# price column adder
goods$price <- price
goods # code        name su     dan   price


# table 생성 
query <- "create table goods_manager(code int, name varchar(50), su int, dan int, price int)"
dbSendUpdate(conn, query)

# goods 변수 내용을 goods_manager 테이블에 저장하기 
dbWriteTable(conn, 'goods_manager', goods)
dbGetQuery(conn, 'select * from goods_manager')

# table 삭제 
query <- "drop table goods_manager"
dbSendUpdate(conn, query)

# data.csv 파일 읽어오기 
dataset <- read.csv(file.choose()) # file 선택기 
str(dataset) # 'data.frame':	22 obs. of  3 variables:
date <- dataset$Date
date # 20150101 -> 2015-01-01 -> Date 생성 

library(stringr)
year <- str_sub(date, 1, 4) # 2015
month <- str_sub(date, 5, 6) # 01
day <-  str_sub(date, 7, 8) # 01
year; month; day

sDate <- str_c(year,month,day, sep="-") # 2015-01-01
sDate
# 문자열 -> 날짜형 형변 
sDate <- as.Date(sDate, '%Y-%m-%d')
sDate
class(sDate) # "Date"

# Date 칼럼을 교체 
dataset$Date <- sDate
dataset

# 문1) dataset 변수 내용을 다음과 같이 테이블에 저장하시오.
# table명 : customer
# 칼럼(자료형) : udate(date), id(int), buy(int)
# dataset -> customer 테이블에 저장 
query <- "create table customer(udate date, id int, buy int)"
dbWriteTable(conn, 'customer', dataset)

# 문2) 고객별 구매수량 합계를 구하고, 막대차트로 시각화하시오.
dbGetQuery(conn, 'select * from customer')
query <- "select Customer_ID, sum(Buy) from customer group by Customer_ID"
result <- dbGetQuery(conn, query)
result 

barplot(result$`sum(Buy)`, col = rainbow(nrow(result)), 
        main = '고객별 구매수량 현황', 
        names.arg = result$Customer_ID)

# 문3) 구매날짜별 구매수량 합계를 구하고, 막대차트로 시각화하시오.
query <- "select Date, sum(Buy) from customer group by Date"
result2 <- dbGetQuery(conn, query)
result2 

barplot(result2$`sum(Buy)`, col = rainbow(nrow(result2)), 
        main = '구매 날짜별 구매수량 현황', 
        names.arg = result2$Date)

# db 연결 종료
dbDisconnect(conn)

