#chap06_2 DataHandling
#####################################
## Chapter06. 데이터 조작 
#####################################

# 1. plyr 패키지 활용 - 데이터 병합

install.packages('plyr')
library(plyr) # 패키지 로딩

# 병합할 데이터프레임 셋 만들기
x = data.frame(ID = c(1,2,3,4,5), height = c(160,171,173,162,165))
y = data.frame(ID = c(5,4,1,3,2), weight = c(55,73,60,57,80))

# 1) join() : plyr패키지 제공 함수
z <- join(x, y, by='ID') # ID컬럼으로 조인
z

x = data.frame(ID = c(1,2,3,4,6), height = c(160,171,173,162,180))
y = data.frame(ID = c(5,4,1,3,2), weight = c(55,73,60,57,80))

# 2_ left 조인
z <- join(x, y, by='ID') # ID컬럼으로 left 조인(왼쪽 변수 기준)
z
z <- join(x, y,by='ID', type='inner') # type='inner' : 값이 있는 것만 조인
z
z <- join(x,y,by='ID', type='full') # type='full' : 모든 항목 조인
z

# 3) key값으로 병합하기

x = data.frame(key1 = c(1,1,2,2,3),  key2 = c('a','b','c','d','e'),
               val1 = c(10,20,30,40,50))
y = data.frame(key1 = c(3,2,2,1,1),  key2 = c('e','d','c','b','a'),
               val2 = c(500,400,300,200,100))

join(x, y, by=c('key1', 'key2'))

########################
#aqpply() vs tapply()
########################

# 1) apply(data.frame/matrix,1/2,FUNC)
apply(iris[,-5],2,mean)

# 2) tapply(dataset,집단변수(범주형변수),FUNC)
names(iris)
#[1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
tapply(iris$Sepal.Length,iris$Species,mean)
tapply(iris$Sepal.Width,iris$Species,sum)

# 2. ddply() : plyr 패키지 제공 함수
# 형식) ddply(dataframe, .(집단변수), 요약집계, 변수명=함수(변수))

# 꽃의 종류별(Species)로 꽃받침 길이(Sepal.Length)의 평균 계산
dp <- ddply(iris, .(Species), summarise, 
            avg = mean(Sepal.Length),
            tot = sum(Sepal.Length),
            var = var(Sepal.Length),
            sd = sd(Sepal.Length))
dp
class(dp) #  "data.frame"

# plyr 패키지 제공 data set
data(baseball)  
str(baseball) # 'data.frame':	21699 obs. of  22 variables:
#$ id   : chr  "ansonca01" -> 선수 id
#$ year : int  1871 1871 1871 -> 활동 년도 
#$ stint: int  1 1 1 1 1 1 1 1 1 1 ...
#$ team : chr  "RC1" "WS3" "FW1" "NY2" ... -> 활동 팀명 
#$ lg   : chr  "" "" "" "" ... -> 리그명
#$ g    : int  25 32 19 33 29 29 29 46 37 25 -> 게임수  
#$ ab   : int  120 162 89 161 128 146 145 217 174 130 -> 베팅수 
#$ r    : int  29 45 15 35 35 40 36 60 26 40 ... -> 도루수 

range(baseball$year) # 1871 2007

head(baseball)
tail(baseball) # benitar01

# 선수별 subset 작성 
benitar01 <- subset(baseball, id=='benitar01')
benitar01

length(benitar01$id) # 17개 

# 활동시기 
min(benitar01$year) # 1994
max(benitar01$year) # 2007
max(benitar01$year) - min(benitar01$year) + 1 # 14

# 활동 팀 
length(unique(benitar01$team)) # 6

# 전체 선수의 수 확인 
length(baseball$id) # 21699
length(unique(baseball$id)) # 1228

# id를 기준으로 각 선수의 시작 년도(start_year) : min() 적용 구하기 
# head() : 6줄 확인  
head(ddply(baseball, .(id), summarise, start_year = min(year)))

# id를 기준으로 시작년도(start_year), 마지막 년도(end_year)
head(ddply(baseball, .(id), summarise, start_year = min(year),
           end_year= max(year)))

# id를 기준으로 각 선수의 활동년도를 출력하시오.
head(ddply(baseball, .(id), summarise, 
           start_year = min(year),
           end_year= max(year),
           cyear = end_year - start_year + 1))


################################################
# 2. dplyr 패키지 활용
################################################
# plyr - java, dplyr - c++

# dplyr 패키지와 데이터 셋 hflight 설치
install.packages(c("dplyr", "hflights"))
library(dplyr)
library(hflights) 

# 차원 보기
dim(hflights) # 227496     21

# 1. tbl_df() 함수 : 데이터셋 화면창 안에서 한 눈에 파악할 수 있는 데이터 구성
hflights
hflights_df <- tbl_df(hflights)
hflights_df


# 2. filter(dataframe, 조건1, 조건2)함수를 이용한 데이터 추출
# 1월 1일 데이터 추출
filter(hflights_df, Month == 1 & DayofMonth == 1) # , == AND(&)

# 1월 혹은 2월 데이터 추출
filter(hflights_df, Month == 1 | Month == 2) # OR

# 3. arrange()함수를 이용한 정렬(1.년도, 2.월, 3.도착시간) - 오름차순 
arrange(hflights_df, Year, Month, ArrTime )

# Month 기준 내림차순 정렬 - desc(변수)
arrange(hflights_df, desc(Month))

# 4. select()함수를 이용한 열 조작
select(hflights_df, Year, Month, DayOfWeek, ArrDelay)  # 4개 칼럼 선택

# 칼럼의 범위 지정 : Year~DayOfWeek 선택
select(hflights_df, Year:DayOfWeek)

# 칼럼의 범위 제외 : Year부터 DayOfWeek 제외
select(hflights_df, -(Year:DayOfWeek))

# -------------------------------------------------------------
# <실습문제> Month 기준으로 내림차순 정렬하여  
#            Year, Month, AirTime, ArrDelay 컬럼을 선택하시오.
# -------------------------------------------------------------
select(arrange(hflights_df,desc(Month)),Year,Month,AirTime,ArrDelay)

# 5. mutate() 함수를 이용하여 칼럼 추가(변형-파생변수 만들기)
# 파생변수 : 도착지연 - 출발지연 시간 
mutate(hflights_df, diff = ArrDelay - DepDelay, 
       diff_per_hour = diff/(AirTime/60) )


# 새로 추가된 열을 select() 함수로 보기
select(mutate(hflights_df, diff = ArrDelay - DepDelay, 
              diff_per_hour = diff/(AirTime/60) ), 
       Year, Month, ArrDelay, DepDelay, diff, diff_per_hour)


# 6. summarise()함수를 이용한 집계 - 출발지연시간 평균 
summarise(hflights_df, cnt = n(), delay = mean(DepDelay, na.rm = TRUE))
#n() = nrow(dataset) 전체 관측치 개수 구하는 함수

# 7. group_by(dataframe, 기준변수)함수를 이용한 그룹화
# - 형식) group_by(dataframe, 기준변수)함수

# 예제) 각 항공기별 비행편수가 20편 이상이고, 평균 비행거리가 
#       2,000마일 이하인 경우의 평균 도착지연시간 구하기

# 1) 비행편수를 구하기 위해서 항공기별 그룹화
planes <- group_by(hflights_df, TailNum) # TailNum : 항공기 일련번호
planes

# 2) 항공기별 요약 : 비행편수, 평균 비행거리, 평균 연착시간
planesInfo <- summarise(planes, count = n(), 
                        dist=mean(Distance, na.rm=T), 
                        delay=mean(ArrDelay, na.rm = T))
planesInfo # TailNum count      dist     delay

# 3) planesInfo객체를 대상으로 조건 지정
result <- filter(planesInfo, count >= 20 & dist <= 2000)
head(result)

######################################################
# 3. reshape 패키지 활용
######################################################

#단계 1 : 패키지 설치
install.packages("reshape") 
library(reshape)

#단계 2 : 실습 데이터 셋 가져오기
result <- read.csv("C:/NCS/Rwork/Part-II/reshape.csv", header=FALSE) 
head(result) 

#단계 3 : 칼럼명 변경
result <- rename(result, c(V1="total", V2="num1", V3="num2", V4="num3")) 
head(result) # total num1 num2 num3

data('Indometh') # Indometh 기본 데이터 셋 확인
str(Indometh) 

Indometh # 긴 형식 보기

# 긴 형식 -> 넓은 형식 
wide <- reshape(Indometh, 
                v.names = "conc",
                timevar = "time", idvar = "Subject",
                direction = "wide")
wide

# 넓은 형식 -> 긴 형식 
reshape(wide, direction = "long")


######################################################
# 4. reshape2 패키지 활용
######################################################

install.packages('reshape2')
library(reshape2)

# 1. dcast()함수 이용 : 긴 형식 -> 넓은 형식 변경
# - '긴 형식'(Long format)을 '넓은 형식'(wide format)으로 모양 변경

data <- read.csv("C:\\NCS\\Rwork\\Part-II\\data.csv")
data

# data.csv 데이터 셋 구성 - 22개 관측치, 3개 변수
# Date : 구매날짜
# Customer : 고객ID
# Buy : 구매수량

# (1) '넓은 형식'(wide format)으로 변형
# 형식) dcast(데이터셋, 앞변수~뒤변수, 함수)
# 앞변수 : 행 구성, 뒷변수 : 칼럼 구성

wide <- dcast(data, Customer_ID ~ Date, sum)
wide 

# 2. 파일에 저장 및 읽기  
setwd("c:/NCS/Rwork/Part-II")
write.csv(wide, 'wide.csv', row.names=FALSE) # 행 번호 없음

wide <- read.csv('wide.csv')
colnames(wide) <- c('Customer_ID','day1','day2','day3','day4','day5','day6','day7')
wide

# 3. melt() 함수 이용 : 넓은 형식 -> 긴 형식 변경
#   형식) melt(데이터셋, id='열이름 변수')

# - 긴 형식 변경
long <- melt(wide, id='Customer_ID') 
long
# id변수를 기준으로 넓은 형식이 긴 형식으로 변경


# 4. 칼럼명 수정
name <- c("Customer_ID", "Date", "Buy")
colnames(long) <- name   
head(long)

# 1:n -> 1:1 파생변수 
library(reshape2)
data("smiths")
smiths

# wide -> long
long<-melt(smiths, id='subject')
long

#long -> wide
wide<-dcast(long,subject~...)
wide

# 3차원 형식으로 변경
data('airquality') # airquality  New York Air Quality Measurements
airquality
str(airquality)

# 칼럼명 대문자 일괄 변경
names(airquality) <- toupper(names(airquality)) # 칼럼명 대문자 변경
head(airquality)

# 월과 일 칼럼으로 나머지 4개 칼럼을 묶어서 긴 형식 변경 
air_melt <- melt(airquality, id=c("MONTH", "DAY"), na.rm=TRUE)
head(air_melt) # month day variable value

# 일과 월 칼럼으로 variable 칼럼을 3차원 형식으로 분류   
names(air_melt) <- tolower(names(air_melt)) # 칼럼명 소문자 변경
acast<- acast(air_melt, day ~ month ~ variable)
acast

# 월 단위 variable(대기관련 칼럼) 칼럼 합계
acast(air_melt, month ~ variable, sum, margins = TRUE)  
