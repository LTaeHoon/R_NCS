#chap07_DataPreprocessing.R

#####################################
## Chapter07. 데이터 전처리 
#####################################

# 1. 탐색적 데이터 조회 

# 실습 데이터 읽어오기

setwd("C:/Rwork/Part-II")
dataset <- read.csv("dataset.csv", header=TRUE) # 헤더가 있는 경우
# dataset.csv - 칼럼과 척도 관계 

# 1) 데이터 조회
# - 탐색적 데이터 분석을 위한 데이터 조회 

# (1) 데이터 셋 구조
names(dataset) # 변수명(컬럼)
attributes(dataset) # names(), class, row.names
str(dataset) # 데이터 구조보기
dim(dataset) # 차원보기 : 300 7
nrow(dataset) # 관측치 수 : 300
length(dataset) # 칼럼수 : 7 

# (2) 데이터 셋 조회
# 전체 데이터 보기
dataset # print(dataset) 
View(dataset) # 뷰어창 출력

# 칼럼명 포함 간단 보기 
head(dataset) 
tail(dataset) 

# (3) 칼럼 조회 
# 형식) dataframe$칼럼명   
dataset$age 
dataset$resident
length(dataset$age) # data 수-300개 
# 조회결과 변수 저장
x <- dataset$gender 
y <- dataset$price
x;y
plot(x, y) # 성별과 가격분포 - 극단치 발견 

# 형식) dataframe["칼럼명"] - $기호 대신 [""]기호 사용
dataset["gender"] 
dataset["price"]

# 형식) dataframe[색인] : 색인(index)으로 원소 위치 지정 
dataset[2] # 두번째 컬럼
dataset[6] # 여섯번째 컬럼
dataset[3,] # 3번째 관찰치(행) 전체
dataset[,3] # 3번째 변수(열) 전체

# dataset에서 2개 이상 칼럼 조회
dataset[c("job","price")]
dataset[c(2,6)] 

dataset[c(1,2,3)] 
dataset[c(1:3)] 
dataset[c(2,4:6,3,1)] 


# 2. 결측치(NA) 처리

summary(dataset$price) 
sum(dataset$price) # NA 출력

# 결측데이터 제거 방법1
sum(dataset$price, na.rm=T) # 2362.9

# 결측데이터 제거 방법2 
price2 <- na.omit(dataset$price) 
sum(price2) # 2362.9
length(price2) # 270 -> 30개 제거

# 결측데이터 처리(0으로 대체)
x <- dataset$price # price vector 생성 
x[1:30] # 5.1 4.2 4.7 3.5 5.0
dataset$price2 = ifelse( !is.na(x), x, 0) # 0으로 대체
dataset$price2[1:30]

# 결측데이터 처리(평균으로 대체)
x <- dataset$price # price vector 생성 
x[1:30] # 5.1 4.2 4.7 3.5 5.0
dataset$price3 = ifelse(!is.na(x), x, round(mean(x, na.rm=TRUE), 2) ) # 평균으로 대체
dataset$price3[1:30]

dataset[c('price', 'price2', 'price3')]


# 3. 극단치 발견과 정제

# 1) 범주형 변수 극단치 처리
gender <- dataset$gender
gender

# outlier 확인
hist(gender) # 히스토그램
table(gender) # 빈도수
pie(table(gender)) # 파이 차트

# gender 변수 정제(1,2)
dataset <- subset(dataset, gender==1 | gender==2)
dataset # gender변수 데이터 정제
length(dataset$gender) # 297개 - 3개 정제됨
pie(table(dataset$gender))


# 2) 비율척도 극단치 처리
dataset$price # 세부데이터 보기
length(dataset$price) #300개(NA포함)
plot(dataset$price) # 산점도 
summary(dataset$price) # 범위확인


# price변수 정제
dataset2 <- subset(dataset, price >= 2 & price <= 8)
length(dataset2$price) 
stem(dataset2$price) # 줄기와 잎 도표보기

#age 변수의 데이터 정제와 시각화
summary(dataset2$age)
length(dataset2$age) 
dataset2 <- subset(dataset2, age >= 20 & age <= 69)
length(dataset2$age) 
boxplot(dataset2$age) 


# 4. 코딩변경 
# - 데이터의 가독성, 척도 변경, 최초 코딩 내용 변경을 목적으로 수행

# 1) 가독성을 위한 코딩변경 
# 형식) dataframe$새칼럼명[부울린언식] <- 변경값   
dataset2$resident2[dataset2$resident == 1] <-'1.서울특별시'
dataset2$resident2[dataset2$resident == 2] <-'2.인천광역시'
dataset2$resident2[dataset2$resident == 3] <-'3.대전광역시'
dataset2$resident2[dataset2$resident == 4] <-'4.대구광역시'
dataset2$resident2[dataset2$resident == 5] <-'5.시구군'
dataset2[c("resident","resident2")] # 2개만 지정

dataset2$job2[dataset2$job == 1] <- '공무원'
dataset2$job2[dataset2$job == 2] <- '회사원'
dataset2$job2[dataset2$job == 3] <- '개인사업'

# 2) 연속형 -> 범주형
dataset2$age2[dataset2$age <= 30] <-"청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <=55] <-"중년층"
dataset2$age2[dataset2$age > 55] <-"장년층"
head(dataset2)

# 3) 역코딩 : 긍정순서(5~1)
# 5점 척도 
# 1.매우만족,  ...  5. 매우불만족 -> 6-1, 6-5 -> 5, 1

dataset2$survey
survey <- dataset2$survey
csurvey <- 6-survey # 역코딩
csurvey
survey  # 역코딩 결과와 비교
dataset2$survey <- csurvey # survery 수정
head(dataset2) # survey 결과 확인


# 5. 탐색적 분석을 위한 시각화 

# (1) 데이터셋 가져오기 
setwd("c:\\Rwork\\Part-II")

# (2) 저장된 파일 불러오기/확인
new_data <- read.csv("new_data.csv", header=TRUE)
new_data 
dim(new_data) #  231  15
str(new_data)

# 1) 명목척도(범주/서열) vs 명목척도(범주/서열) 
# - 거주지역과 성별 칼럼 시각화 
resident_gender <- table(new_data$resident2, new_data$gender2)
resident_gender
gender_resident <- table(new_data$gender2, new_data$resident2)
gender_resident

# 성별에 따른 거주지역 분포 현황 
barplot(resident_gender, beside=T, horiz=T,
        col = rainbow(5),
        legend = row.names(resident_gender),
        main = '성별에 따른 거주지역 분포 현황') 
# row.names(resident_gender) # 행 이름 

# 거주지역에 따른 성별 분포 현황 
barplot(gender_resident, beside=T, 
        col=rep(c(2, 4),5), horiz=T,
        legend=c("남자","여자"),
        main = '거주지역별 성별 분포 현황')  

# 2) 비율척도(연속) vs 명목척도(범주/서열)
# - 나이와 직업유형에 따른 시각화 
install.packages("lattice")  # chap09
library(lattice)

# 직업유형에 따른 나이 분포 현황   
densityplot( ~ age, data=new_data, groups = job2,
             plot.points=T, auto.key = T)
# plot.points=T : 밀도, auto.key = T : 범례 

# 3) 비율(연속) vs 명목(범주/서열) vs 명목(범주/서열)
# - 구매비용(연속):x칼럼 , 성별(명목):조건, 직급(서열):그룹   

# (1) 성별에 따른 직급별 구매비용 분석  
densityplot(~ price | factor(gender2), data=new_data, 
            groups = position2, plot.points=T, auto.key = T) 
# 조건(격자) : 성별, 그룹 : 직급 

# (2) 직급에 따른 성별 구매비용 분석  
densityplot(~ price | factor(position2), data=new_data, 
            groups = gender2, plot.points=T, auto.key = T) 
# 조건 : 직급(격자), 그룹 : 성별 

# 4) 비율(연속)2개 vs 명목 
xyplot(price ~ age | factor(gender2), data=new_data) 


# 6.파생변수 생성 - p.182 참조 
# - 기존 변수로 새로운 변수 생성

setwd('C:\\NCS\\Rwork\\Part-II')
user_data <- read.csv('user_data.csv', header = T)
head(user_data) # user_id age house_type resident job 

# 1) 1:1 파생변수 생성 - p.76 참조 
# - 주택 유형 :  0, 아파트 유형 : 1(더미변수 생성) : 주택 유형 파악 가능  
summary(user_data$house_type) # NA확인 - 없음 
table(user_data$house_type)

# 더미변수 생성 
house_type2 <- ifelse(user_data$house_type == 1 | user_data$house_type == 2, 0, 1)
# 결과 확인
house_type2[1:10] 
# 파생변수 추가 
user_data$주거환경 <- house_type2
head(user_data)


# 2) 1:N 파생변수 생성 : p.76 참조 
pay_data <- read.csv('pay_data.csv', header = T)
head(pay_data,10) # user_id product_type pay_method  price
table(pay_data$product_type)

library(reshape2)

# (1) 고객별 상품 유형에 따른 구매금액 합계 파생변수 생성   
product_price <- dcast(pay_data, user_id ~ product_type, sum, na.rm=T) # 행 ~ 열 
head(product_price, 3) # 행(고객 id) 열(상품 타입)

class(product_price) # "data.frame"
# 칼럼명 수정 
names(product_price) <- c('user_id','식료품(1)','생필품(2)','의류(3)','잡화(4)','기타(5)')
head(product_price, 3) # 칼럼명 수정 확인 

# (2) 고객별 지불유형에 따른 구매상품 개수 파생변수 생성 
pay_price <- dcast(pay_data, user_id ~ pay_method, length) # 행 ~ 열 
head(pay_price, 3) # 행(고객 id) 열(상품 타입)

names(pay_price) <- c('user_id','현금(1)','직불카드(2)','신용카드(3)','상품권(4)')
head(pay_price, 3) # 칼럼명 수정 확인 


# (3) 파생변수 추가(data.frame 합치기) 
library(plyr) # 패키지 로딩

# 형식) join(df1, df2, by='column')
user_pay_data <- join(user_data, product_price, by='user_id')
head(user_pay_data,10)

user_pay_data <- join(user_pay_data, pay_price, by='user_id')
user_pay_data[c(1:10), c(1,7:15)]


# (4) 총 구매금액 파생변수 생성(사칙연산 : 지급방법 이용) 
user_pay_data$총구매금액 <- user_pay_data$`식료품(1)` +user_pay_data$`생필품(2)`+user_pay_data$`의류(3)` +
  user_pay_data$`잡화(4)` + user_pay_data$`기타(5)`
user_pay_data[c(1:10), c(1,7:16)]

# 7. 표본 셈플링
# - 전체 데이터 셋을 대상으로 무작위(random)로 관측치를 추출하는 방법  
str(iris) #'data.frame':	150 obs. of  5 variables:

idx <- sample(nrow(iris),nrow(iris)*0.7) #70% 행 추출
idx

train <- iris[idx,] # 70% (model 생성)
train
test <- iris[-idx,]
dim(train) # 105 5
dim(test) # 45 5

choice <- sample(c(1:100), 30) # 1~100
choice # 30개 랜덤 추출 

# 1) 행 번호 추출 
idx <- sample(1:nrow(user_pay_data),30) # 30개 무작위 추출
idx # 추출값 : 행 번호

# 2) 관측치 추출 
result <- user_pay_data[idx, ]
result


#다양한 범위를 지정해서 무작위 셈플링
choice2 <- sample(c(10:50, 70:150, 160:190),30)
choice2


# iris 데이터셋을 대상으로 7:3 비율로 훈련데이터와 검정데이터 셋 생성하기 
iris
str(iris) # 'data.frame':	150 obs. of  5 variables:

idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
train <- iris[idx, ] # 70%
test <- iris[-idx, ] # 30%
dim(train) # 105   5
dim(test) # 45  5

LETTERS # A ~ Z
letters # a ~ z
x <- runif(2600000, min = 0, max = 100)
y <- rep(LETTERS, each=100000)
df <- data.frame(x, y)
head(df)
tail(df)
dim(df) # 2600000     2

# df 대상으로 50개만 샘플링하여 영문자 빈도수를 pie 차트로 시각화 
idx <- sample(nrow(df), 50)
test <- df[idx, ]
pie(table(test$y))


# 8. k겹 교차검정(cross test) 데이터 생성

# k겹 교차검정 데이터 생성 알고리즘
# 1. k 개로 데이터 균등 분(D1,D2,D3 .... Dk) : D1 -> test, D2~Dk :train
# 2. 검정데이터 위치를 하나씩 변경하고, 나머지 데이터를 학습데이터 생성
# 3. 위 과정을 k 번 반복
# # ex) K  test   train
#       k=1 D1     D2~Dk
#       k=2 D2     D1,D3~Dk
#       k=3 D3     D1~D2,D4~Dk

name <- c('a','b','c','d','e','f')
score <- c(90,85,70,85,60,74)
df <- data.frame(Name=name,Score=score)
df

install.packages('cvTools')
library(cvTools)
?cvFolds
# cvFolds(n, K = 5, R = 1,   R은 회전
#         type = c("random", "consecutive", "interleaved"))

cross <- cvFolds(n=6,K=3,R=1,type="random")
cross

# K=3 -> D1(4,5), D2(3,2), D3(6,1)
# K=1 일때 test : D1(4,5) train: D2(3,2), D3(6,1)
# K=2 일때 test : D2(3,2) train: D1(4,5), D3(6,1)
# K=3 일때 test : D3(6,1) train: D1(4,5), D2(3,2)

str(cross)
#List of 5
# $ subsets: int [1:6, 1] 4 3 6 5 2 1 -> key = matrix
# $ which  : int [1:6] 1 2 3 1 2 3    -> key = vector

# Fold=which, Index=subsets
cross$which #[1] 1 2 3 1 2 3
cross$subsets 

#which를 이용하여 subsets 데이터 참조
cross$subsets[cross$which==1,1] #D1(4,5)
cross$subsets[cross$which==2,1] #D2(3,2)
cross$subsets[cross$which==3,1] #D3(6,1)

r=1 # 1회 반복
K=1:3 # 3겹 교차검정
#회전수가 늘어날 경우 바깥쪽에 for문 하나 더 있어야
for(k in K){ # 3회전 반복
  idx <- cross$subsets[cross$which==k,r]
  cat('k=',k,'검정데이터 \n')
  print(df[idx,])
  
  for(i in K[-k]){ # 3*2 = 6회전
    idx <- cross$subsets[cross$which==i,r]
    cat('i=',i,'훈련데이터 \n')
    print(df[idx,])
    } #inner for
}#outer for
