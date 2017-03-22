#chap06_1 dataframe vs datatable

#################################
## data.frame vs data.table
################################

# 1. 다양한 indexing 제공
# 2. 접근 속도 빠름

install.packages("data.table")
library(data.table)

# 1) data.frame 생성
df <- data.frame(x=c(1:3),y=c('one','two','three'))
df
class(df) #"data.frame"

# 2) data.table 생성
dt <- as.data.table(df)
dt
class(dt)
dt2 <- data.table(x=c(1:3),y=c('one','two','three'))
dt2
class(dt2) #"data.table" "data.frame"

# 1. 다양한 indexing 제공
data(iris)
str(iris)
class(iris)

# 1) data.frame indexing
iris[1,'Sepal.Length']# 5.1
iris[1:10,c('Sepal.Length','Sepal.Width')]

# 2) data.table indexing
iris_table <- as.data.table(iris)
class(iris_table)
head(iris_table)
iris_table[1,'Sepal.Length']
iris_table[1:10,c('Sepal.Length','Sepal.Width')]

iris_table[,mean(Sepal.Length)]
iris_table[,list(avg1=mean(Sepal.Length), avg2=mean(Sepal.Width))]

iris_table[,list(avg1=mean(Sepal.Length), avg2=mean(Sepal.Width)),by=Species]

#특수문자 처리 함수 정의
num_pro <- function(str){
  library(stringr)  
  str2 <- str_replace_all(str,'\\,|\\(|\\)|\\$|\\%','') 
  num <- as.numeric(str2)
  return(num)
}

str <-'($123,456%)'
num_pro(str) #123456
num * 2 #250912

#stock.csv 파일 가져오기
stock <- read.csv('c:/NCS/Rwork/Part-I/stock.csv',header = T,stringsAsFactors = F)
str(stock)

head(stock)

# 1) char 칼럼과 num 칼럼
char_col <- stock[,1:6]
num_col <- stock[,7:15]

# 2) 사용자 함수에 적용(num_col)
rnum_col <- apply(num_col,2,num_pro)
head(rnum_col)

# 3) char_col + rnum_col
stock_result <- cbind(char_col,rnum_col)
str(stock_result)
class(stock_result)

head(stock_result)

# Sector : 분야, Market.Cap : 시가총액, P.E : 주가이익, Forward.P.E : 예상 주가이익

## 문1) 숫자컬럼을 대상으로 평균을 구하시오(컬럼단위로)
apply(stock_result[,7:15], 2, mean, na.rm=T)

## 문2) 분야별(Sector) 시가총액,주가이익, 예상 주가이익의 평균을 구하시오.
stock_result_table <- as.data.table(stock_result)
head(stock_result_table)
stock_result_table[,list(avg_mc=mean(Market.Cap,na.rm = T),avg_pe=mean(P.E,na.rm = T),avg_fpe=mean(Forward.P.E,na.rm = T)),by=Sector]
?aggregate
aggregate(stock_result_table$Market.Cap,by=list(stock_result_table$Sector),sum,na.rm=T)

# 2. 접근 속도 빠름

# R에서 제공하는 상수
LETTERS
letters
x <- runif(2600000,min=0,max=100) #0~100
y <- rep(LETTERS,each=100000)

#data.frame 생성
df <-data.frame(x,y)
head(df); tail(df)

# 수행시간
system.time(df[df$y=='Z',]) # y컬럼 내 Z값 조회
# 사용자  시스템 elapsed 
# 0.07    0.03    0.09 (소요시간)

# data.table 생성
dt <- as.data.table(df)

# 검색을 대상을 key
setkey(dt,y) #y컬럼을 검색키로 지정
system.time(dt[J('Z'),])  # y컬럼 내 Z값 조회
