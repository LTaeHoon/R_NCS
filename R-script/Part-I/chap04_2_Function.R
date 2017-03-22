#####################################
## Chapter04_2. 함수 
#####################################
# 사용자 정의함수와 내장함수 

# 1. 사용자 정의함수

# 매개변수가 없는 함수 예
f1 <- function(){
  cat("매개변수가 없는 함수")
}

f1() # 함수 호출 

#매개변수가 있는 함수 예
f2 <- function(x){ # 가인수 
  cat("x의 값 = ",x, "\n") # \n 줄바꿈
  print(x) # 변수만 사용
}

f2(10) # 실인수 


# 리턴값이 있는 함수 예
f3 <- function(x, y){
  add <- x + y # 덧셈 
  return(add) # 결과 반환 
}

add <- f3(10, 20)
add


# 통계량을 구하는 함수 
stat <- function(FUNC, data){
  switch (FUNC,
          SUM = sum(data),
          AVG = mean(data),
          VAR = var(data)
  )
}

data <- c(1:10)
stat('SUM', data) # 55
stat('AVG', data) # 5.5
stat('VAR', data) # 9.166667

#파일 불러오기
setwd("c:/Rwork/Part-I")
test <- read.csv("test.csv", header=TRUE)
head(test)

# A 칼럼 요약통계량, 빈도수 구하기 
summary(test) # 요약통계량
table(test$A) # A변수 대상 빈도수 
max(test$A) # 최고 빈도수 
min(test$A) # 최소 빈도수 

# 각 칼럼 단위 요약통계량과 빈도수 구하기
data_pro <- function(x){
  
  for (idx in 1 : length(x)){
    cat(idx,'번째 칼럼의 빈도분석 결과')
    print(table(x[idx]))
    cat('\n')
  }
  for (idx in 1 : length(x)){
    f <- table(x[idx])
    cat(idx,'번째 칼럼의 최댓값/최솟값\n')
    cat("max =", max(f), "min =", min(f), '\n')
  }
}

data_pro(test) #함수 호출


#파타고라스 정의 증명- 식 : a^2+b^2=c^2
pytha <- function(s,t){
  a <- s^2 - t^2
  b <- 2*s*t
  c <- s^2 + t^2
  cat("피타고라스의 정리 : 3개의 변수 : ",a,b,c)
}

pytha(2,1) # s,t는 양의 정수 -> 3 4 5

#결측치(NA) 데이터 처리
data<- c(10,20,5,4,40,7,NA,6,3,NA,2,NA) # 97
data
mean(data, na.rm = T) # NA


#결측치 데이터 처리 함수
na <- function(x){
  #1차 : NA 제거 
  print(x)
  print( mean(x, na.rm = T) )
  
  #2차 : NA를 0으로 대체  
  data = ifelse(!is.na(x), x, 0) # NA이면 0으로 대체
  print(data)
  print(mean(data))
  
  # 3차 : NA를 평균으로 대체 
  data2 = ifelse(!is.na(x), x, round(mean(x, na.rm=TRUE), 2) ) # 평균으로 대체 
  print(data2)
  print(mean(data2))
}
na(data) #함수 호출
# 결측치를 무조건 제거하면 정확한 통계량을 얻을 수 없으며, 데이터가 손실될 수 있다. 

################################
### 몬테카를로 시뮬레이션 
################################
# 현실적으로 불가능한 문제의 해답을 얻기 위해서 난수의 확률분포를 이용하여 
# 모의시험으로 근사적 해를 구하는 기법

# 동전 앞/뒤 난수 확률분포 함수 
coin <- function(n){
  r <- runif(n, min=0, max=1)
  #print(r) # n번 시행 
  
  result <- numeric()
  for (i in 1:n){
    if (r[i] <= 0.5)
      result[i] <- 0 # 앞면 
    else 
      result[i] <- 1 # 뒷면
  }
  return(result)
}
coin(10) # 시행 횟수 : 10회  

# 몬테카를로 시뮬레이션 
montaCoin <- function(n){
  cnt <- 0
  for(i in 1:n){
    cnt <- cnt + coin(1) # 동전 함수 호출 
  }
  result <- cnt / n
  return(result)
}

montaCoin(10) # 0.3

montaCoin(30) # 0.5666667

montaCoin(100) # 0.53

montaCoin(1000) #  0.51

montaCoin(10000) # 0.5033


###############################
## 2. 주요 R 내장 함수 
###############################

seq(-2, 2, by=.2) # 0.2씩 증가
seq(length=10, from=-5, by=.2) # -5부터 10개 생성 
rnorm(20, mean = 0, sd = 1) # 정규분포를 따르는 20개 데이터 생성
runif(20, min=0, max=100) # 0~100사이의 20개 난수 생성
sample(0:100, 20) # 0~100사이의 20개 sample 생성
vec<-1:10
min(vec)
max(vec)
range(vec)
mean(vec) # 평균
median(vec) # 중위수
sum(vec) 
prod(vec) # 데이터의 곱

factorial(5) # 팩토리얼=120
abs(-5)  # 절대값
sd(rnorm(10)) # 표준편차 구하기 
table(vec) # 빈도수 
sqrt(16) # 4 
4^2 # 16
# 나머지 구하기
5%%3 # 2
6%%2 # 0

log(10) # 10의 자연로그(밑수가 e)
log10(10) # 10의 일반로그(밑수 10) 


getwd()
setwd("c:/Rwork/Part-I")
excel <- read.csv("excel.csv", header=TRUE)
head(excel, 10) 

#colMeans()함수 : 각 열의 평균 계산
colMeans(excel[1:5])
rowMeans(excel[1:5])
summary(excel) # 요약통계량 = 기술통계량  


###################################
### 반올림 관련 함수 
###################################
x <- c(1.5, 2.5, -1.3, 2.5)
x
round(mean(x)) # 1.3 -> 1
ceiling(mean(x)) # x보다 큰 정수 
floor(mean(x)) # 1보다 작은 정수 

# 중위수 구하기 
x1 <- c(2,3,4,1,5,6) # 짝수 = ([n/2] + [n/2+1]) / 2
sx1 <- sort(x1)
sx1 # 1 2 3 4 5 6
median(sx1) # 3.5 # [3] + [4] / 2 -> 3.5

x2 <- c(2,3,4,1,5) # 홀수 = ceiling([n/2]) 
sx2 <- sort(x2)
sx2 # 1 2 3 4 5
median(sx2) # 3


###################################
## 난수 생성과 확률분포 
###################################

# 1. 정규분포를 따르는 난수 생성 
# 형식) rnorm(n, mean=0, sd = 1)
n <- 1000
r <- rnorm(n, mean = 0, sd = 1) 
r
hist(r) # 대칭성 

# 2. 균등분포를 따르는 난수 생성 
# 형식) runif(n, min, max)
r2 <- runif(n, min=0, max=1) # 0 < r2 < 1
r2
hist(r2)


# 3. 이항분포를 따르는 난수 생성 
# 형식) rbinom(n, sample size, prop)
# sample size를 대상으로 나올 수 있는 확률값 지정 
# 만약 sample 추출이 실패하면 0으로 출력 

set.seed(123) # seed()함수를 실행하면 같은 값으로 난수 생성 
n <- 10
r3 <- rbinom(n, 1, 0.5) # 1개 size에서 1/2(0.5)확률로 난수 정수 생성 
r3 #  0 1 0 1 1 0 1 1 1 0 

r3 <- rbinom(n, 1, 0.25) # 1개 size에서 1/4(0.25)확률로 난수 정수 생성 
r3 #  1 0 0 1 0 0 0 0 0 0 

r3 <- rbinom(n, 3, 0.5) # 3개 size에서 1/2(0.5)0.5확률로 난수 정수 생성  
r3 #  3 1 2 2 0 3 1 0 1 3 

# 종자값(seed)을 지정하여 동일한 난수발생
rnorm(5,mean=0,sd=1)

set.seed(123)
rnorm(5,mean=0,sd=1)
#[1] -0.56047565 -0.23017749  1.55870831  0.07050839  0.12928774

set.seed(345)
rnorm(5,mean=0,sd=1)
#-0.78490816 -0.27951436 -0.16145790 -0.29059656 -0.06753159

# 4. sample(n,size)
sample(20:100,5) #[1] 41 95 33 68 93

no<-c(1,2,3,4,5)
score <- c(10,30,40,10,50)
df <- data.frame(no,score)
df

idx <- sample(nrow(df),nrow(df)*0.7) #70
#[1] 1 4 5

train <- df[idx,]
train
test <-df[-idx,]
test

############################
## [추가] 특수문자 처리 함수
###########################

library(stringr)
str <- '$125,456%'

str2 <- str_replace_all(str,'\\,|\\$|\\%','')
str2
num <- as.numeric(str2)
num*2  #250912

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


## 문2) y1980~y2015 각 주별 1인당 소득의 평균과 합계를 구하시오.
## 주별 1인당 소득자료 가져오기
setwd('c:/NCS/Rwork/output')
info <- read.csv('info.csv', header = T)
head(info)

# 단계 1 : State 컬럼과 년도 컬럼을 구분한다.
# 단계 2 : 년도 칼럼 숫자 형변환 : num_pro 함수 적용
# 단계 3 : 각 주별 1인당 소득의 평균과 합계를 칼럼 계산
# 단계 4 : State, 년도 칼럼, 평균, 합계 칼럼을 합치기 -> data.frame 생성


# 단계 1
state <- info[,1]
year <- info[,-1]
state
year
class(year)

# 단계 2
year <- apply(year, 2, num_pro)
year
class(year)


# 단계 3 
state_avg <- rowMeans(year)
state_tot <- rowSums(year)

# 단계 4 :  
info.result <- data.frame(state,year,state_avg,state_tot)
head(info.result)



