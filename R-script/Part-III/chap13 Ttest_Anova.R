# chap13 Ttest_Anova
####################################
#     점 추정과 구간 추정                                      #
####################################
# 추정(estimation) : 표본을 통해서 모집단을 확률적으로 추측   
# 검정통계량 : 표본에 의해서 계산된 통계량(표본평균, 표본표준편차)
# 모수 : 모집단에 의해서 나온 통계량(모평균, 모표준편차)  

# 1) 점 추정 : 제시된 한 개의 값과 검정통계량을 직접 비교하여
#    가설 기각유무를 결정 
# ex) 우리나라 중학교 2학년 남학생 평균키는 165.2cm로 추정

# 2) 구간 추정 : 신뢰구간과 검정통계량을 비교하여 가설 기각유무 결정 
# 신뢰구간 : 오차범위에 의해서 결정된 하한값과 상한값의 범위 
# ex) 우리나라 중학교 2학년 남학생 평균키는 164.5 ~ 165.5cm로 추정

#####################################
## 신뢰구간에 의한 구간 추정 예
#####################################

# 모집단에서 평균 100, 표준편차는 10을 갖는 표본 1000 추출
n <- 1000
x <- rnorm(n,mean=100,sd=10)
length(x)
hist(x)
shapiro.test(x) #p-value = 0.2831 >= 0.05 귀무가설 채택 따라서 x는 정규분포 이다

# 평균차이 검정(t-test)
# 1) 귀무가설 기각

t.test(x,mu=95)

# 귀무가설 : 평균 95와 차이가 없다.  : p>= 알파
# 대립가설 : 평균 95와 차이가 있다. : p<알파

# t = 15.485, df = 999, p-value < 2.2e-16 <0.05 귀무가설 기각
# 95 percent confidence interval:
#   99.18532 100.40000 : 신뢰구간  = 채택역(귀무가설을 채택할 수 있는 지역)

# 1) 귀무가설 채택

t.test(x,mu=99.22)
#t = 1.8503, df = 999, p-value = 0.06457 >= 0.05 : 귀무가설 채택
#95 percent confidence interval:
#  99.18532 100.40000
# 귀무가설 : 평균 99.22와 차이가 없다.  : p>= 알파
# 대립가설 : 평균 99.22와 차이가 있다. : p<알파


# 3) 신뢰수 향상 -> 채택역 확대
# 95%(통상) -> 99%(의생명)
t.test(x,mu=99.22, conf.level = 0.99) #99%
#t = 1.8503, df = 999, p-value = 0.06457
#99 percent confidence interval:
#98.99392 100.59140 -> 신뢰구간 확대(신뢰수준) : 채택역 확대 , 그만큼 귀무가설 기각을 어렵게 함

# 알파(유의수준) 신뢰수준
# 0.01(1%)   ->   99% (의생명)
# 0.05(5%)   ->   95% (사회과학)
# 0.1(10%)   ->   90% (특수)

########################################
##  표준화 vs 정규화
########################################

# 1. 정규분포 표준화 -> 표준정규분포
# mean = 0, sd = 1
n <- 1000
x <- rnorm(n,mean=100,sd=10)
hist(x)
shapiro.test(x) # p-value = 0.398>0.05

# 표준화 공식 (z) = (x - mu)/ sd(x)
mu <- mean(x)
z = (x - mu) /sd(x) 
mean(z) #[1] -3.388461e-16 0의 근사값
sd(z) #[1] 1
#표준화 함수 scale()
z2 <- scale(x)
mean(z2);sd(z2)
# [1] -3.388461e-16
# [1] 1

# 정규화 : 다양한 특징을 갖는 데이터를 일정한 범위로 일치시킴 

# (1) log() - 자연로그
cost <- c(1.5,5.1,4.2,3.5,4.7,3.5,123)
range(cost) #1.5 123.0
log_cost <- log(cost + 1) #0이 있는 경우 처리하기 위해 +1을 해준다
range(log_cost) # [1] 0.9162907 4.8202816

# (2) 정규화 함수 정의
summary(iris[1:4])

nor <- function(x){
  n<-(x-min(x))/(max(x)-min(x))
  return(n)
}

result<-apply(iris[1:4],2,nor)
class(result)
summary(result)
#
#############################################
# 추론통계분석 - 1-1. 단일집단 비율차이 검정
#############################################
# - 단일 집단의 비율이 어떤 특정한 값과 같은지를 검증

# 1. 실습데이터 가져오기
getwd()
setwd("c:/NCS/Rwork/Part-III")
data <- read.csv("one_sample.csv", header=TRUE)
head(data)
x <- data$survey


# 2. 빈도수와 비율 계산
summary(x) # 결측치 확인
length(x) # 150개
table(x) # 0:불만족(14), 1: 만족(136) 

install.packages("prettyR")
library(prettyR) # freq() 함수 사용
freq(x) 

# 3. 가설검정 

# 형식) binom.test(불만족수, 만족수, p = 확률)
# 연구가설(H1) : 기존 2014년도 고객 불만율과 2015년도 CS교육 후 불만율에 차이가 있다.
#  귀무가설(H0) : 기존 2014년도 고객 불만율과 2015년도 CS교육 후 불만율에 차이가 없다.
# 2014년도 114 전화번호 안내고객을 대상으로 불만을 갖는 고객은 20%였다. 이를 개선하기 위해서 2015년도 CS교육을 실시한 후 150명 고객을 대상으로 조사한 결과 14명이 불만을 갖고 있었다. 기존 20% 보다 불만율이 낮아졌다고 할 수 있는가?

# 1) 불만족율 기준 검정
# 양측검정
binom.test(c(14,136), p=0.2) # 기존 20% 불만족율 기준 검증 실시
binom.test(c(14,136), p=0.2, alternative="two.sided", conf.level=0.95)

# 방향성이 있는 연구가설 검정 : 2015 > 2014  : p-value = 0.9999 >=0.05
binom.test(c(14,136), p=0.2, alternative="greater", conf.level=0.95)

# [실습]방향성이 있는 연구가설 검정  : 2015 < 2014  p-value = 0.0003179 <0.05
binom.test(c(14,136), p=0.2, alternative="less", conf.level=0.95)

#############################################
# 추론통계분석 - 1-2. 단일집단 평균차이 검정
#############################################
# - 단일 집단의 평균이 어떤 특정한 값과 차이가 있는지를 검증

# 1. 실습파일 가져오기
setwd("c:/NCS/Rwrok/Part-III")
data <- read.csv("one_sample.csv", header=TRUE)
str(data) # 150
head(data)
x <- data$time
head(x)

# 2. 기술통계량 평균 계산
summary(x) # NA-41개
mean(x) # NA
mean(x, na.rm=T) # NA 제외 평균(방법1)

x1 <- na.omit(x) # NA 제외 평균(방법2)
mean(x1)

# 3. 정규분포 검정
# 정규분포(바른 분포) : 평균에 대한 검정 
# 정규분포 검정 귀무가설 : 정규분포와 차이가 없다.
# shapiro.test() : 정규분포 검정 함수


shapiro.test(x1) # 정규분포 검정 함수(p-value = 0.7242) 

# 4. 가설검정 - 모수/비모수
# 정규분포(모수검정) -> t.test()
# 비정규분포(비모수검정) -> wilcox.test()

# 1) 양측검정 - 정제 데이터와 5.2시간 비교
t.test(x1, mu=5.2) 
t.test(x1, mu=5.2, alter="two.side", conf.level=0.95) # p-value = 0.0001417
# 해설 : 평균 사용시간 5.2시간과 차이가 있다.

# 2) 방향성이 있는 연구가설(=대립가설) 검정 : A > 국내 -> p-value = 7.083e-05 < 0.05
t.test(x1, mu=5.2, alter="greater", conf.level=0.95) 
# A < 국내 -> p-value = 0.9999 > 0.05 귀무가설 채택 : A가 평균사용시간이 크다.
t.test(x1, mu=5.2, alter="less", conf.level=0.95) 

#############################################
# 추론통계분석 - 2-1. 두집단 비율차이 검정 (비모수 검정이기 때문에 전제조건이 필요없음)
#############################################

# 1. 실습데이터 가져오기
setwd("c:/NCS/Rwork/Part-III")
data <- read.csv("two_sample.csv", header=TRUE)
data
head(data) # 변수명 확인


# 2. 두 집단 subset 작성
data$method # 1, 2 -> 노이즈 없음
data$survey # 1(만족), 0(불만족)

# - 데이터 정체/전처리
x<- data$method # 교육방법(1, 2) -> 노이즈 없음
y<- data$survey # 만족도(1: 만족, 0:불만족)
x;y

# 1) 데이터 확인
# 교육방법 1과 2 모두 150명 참여
table(x) # 1 : 150, 2 : 150
# 교육방법 만족/불만족
table(y) # 0 : 55, 1 : 245

# 2) data 전처리 & 두 변수에 대한 교차분석
table(x, y, useNA="ifany")  #NA 개수를 파악하고 싶다면 useNA ="ifany" 사용
?table

# 3. 두집단 비율차이검증 - prop.test()

# 양측가설 검정
prop.test(c(110,135), c(150, 150)) # 방법1와 방법2 만족도 비율 차이 검정 
prop.test(c(110,135), c(150, 150), alternative="two.sided", conf.level=0.95)
#p-value = 0.0003422 <0.05 연구가설 채택 두가지 교육방법에 따라 만족도의 차이가 있다. 
# # 방향성이 있는 연구가설 검정  : 첫번째 교육 방법 > 두번째 교육 방법 p-value = 0.9998 >0.05 귀무가설 채택 : 두번째 교육방법의 만족도가 높다.
prop.test(c(110,135), c(150, 150), alternative="greater", conf.level=0.95)
# 첫번째 교육 방법 < 두번째 교육 방법 p-value = 0.0001711 <0.05 연구가설 채택 
prop.test(c(110,135), c(150, 150), alternative="less", conf.level=0.95)

#############################################
# 추론통계분석 - 2-2. 두집단 평균차이 검정
#############################################

# 1. 실습파일 가져오기
data <- read.csv("c:/NCS/Rwork/Part-III/two_sample.csv", header=TRUE)
data 
head(data) #4개 변수 확인
summary(data) # score - NA's : 73개

# 2. 두 집단 subset 작성(데이터 정제,전처리)
result <- subset(data, !is.na(score), c(method, score))
dataset <- result[c('method', 'score')]
head(dataset)
summary(dataset)
table(dataset$method)


# 3. 데이터 분리
# 1) 교육방법 별로 분리
method1 <- subset(dataset, method==1)
method2 <- subset(dataset, method==2)

# 2) 교육방법에서 점수 추출
method1_score <- method1$score
method2_score <- method2$score

# 3) 기술통계량 
length(method1_score); # 109
length(method2_score); # 118

# 4. 분포모양 검정 : 두 집단의 분포모양 일치 여부 검정

var.test(method1_score, method2_score) # p-value = 0.3002 -> 차이가 없다.
# 동질성 분포 : t.test()
# 비동질성 분포 : wilcox.test()

# 5. 가설검정 - 두집단 평균 차이검정
t.test(method1_score, method2_score)
t.test(method1_score, method2_score, alter="two.sided", conf.int=TRUE, conf.level=0.95)
# p-value = 0.0411 - 두 집단간 평균에 차이가 있다.
?t.test
# # 방향성이 있는 연구가설 검정 : a > b p-value = 0.9794>0.05 귀무가설 채택
t.test(method1_score, method2_score, alter="greater", conf.int=TRUE, conf.level=0.95)
# 0.02055 < 0.05 연구가설 채택 method1 < method2
t.test(method1_score, method2_score, alter="less", conf.int=TRUE, conf.level=0.95)


################################################
# 추론통계분석 - 2-3. 대응 두 집단 평균차이 검정
################################################
# 조건 : A집단  독립적 B집단 -> 비교대상 독립성 유지
# 대응 : 표본이 짝을 이룬다. -> 한 사람에게 2가지 질문
# 사례) 다이어트식품 효능 테스트 : 복용전 몸무게 -> 복용후 몸무게 

# 1. 실습파일 가져오기
getwd()
setwd("c:/NCS/Rwork/Part-III")
data <- read.csv("paired_sample.csv", header=TRUE)

head(data)
# 2. 두 집단 subset 작성

# 1) 데이터 정제
result <- subset(data, !is.na(after), c(before,after))
dataset <- result[ c('before',  'after')]
dataset

# 2) 적용전과 적용후 분리
before <- dataset$before# 교수법 적용전 점수
after <- dataset$after # 교수법 적용후 점수
before; after

# 3) 기술통계량 
length(before) # 100
length(after) # 100
mean(before) # 5.145
mean(after, na.rm = T) # 6.220833 -> 1.052  정도 증가


# 3. 분포모양 검정 
var.test(before, after, paired=TRUE)  #p-value = 0.7361 >0.05 귀무가설 채택 
# 동질성 분포 : t.test()
# 비동질성 분포 : wilcox.test()

# 4. 가설검정
t.test(before, after, paired=TRUE) # p-value < 2.2e-16 

# 방향성이 있는 연구가설 검정 
t.test(before, after, paired=TRUE,alter="greater",conf.int=TRUE, conf.level=0.95) 
#p-value = 1 -> x을 기준으로 비교 : x가 y보다 크지 않다.

#  방향성이 있는 연구가설 검정
t.test(before, after, paired=TRUE,alter="less",conf.int=TRUE, conf.level=0.95) 
# p-value < 2.2e-16 -> x을 기준으로 비교 : x가 y보다 적다.


############################################
# 추론통계분석 - 3-1. 세집단 비율차이 검정
############################################
# - 세집단 간 비율차이 검정

# 1. 파일가져오기 
getwd()
setwd("c:/NCS/Rwork/Part-III")
data <- read.csv("three_sample.csv", header=TRUE)
data

# 2. 세집단 subset 작성(데이터 정제,전처리) 
method <- data$method 
survey<- data$survey
method
survey 

# 3.기술통계량(빈도분석)
table(method, useNA="ifany") # 50 50 50 -> 3그룹 모두 관찰치 50개
table(method, survey, useNA="ifany") # 그룹별 클릭수 : 1-43, 2-34, 3-37


# 4. 세집단 비율차이 검정
# prop.test(그룹별 빈도, 그룹수) -> 집단이 늘어나도 동일한 함수 사용-땡큐
prop.test(c(34,37,39), c(50,50,50)) # p-value = 0.5232 -> 귀무가설 채택


############################################
# 추론통계분석 - 3-2. 세집단 평균차이 검정
############################################
# 세 집단 간 평균차분 분석

# 일원배치 분산분석 적용 예
# 쇼핑몰 고객의 연령대 별, 시간대 별 구매현황 분석

names(iris) # "Sepal.Length" "Species" : 꽃의 종(3가지)
# aov(y ~ x, data='dataset')

# 전제조건
bartlett.test(Sepal.Length~Species,data=iris)
#p-value = 0.0003345 <0.05 동질하지 않다.

model <- aov(Sepal.Length ~ Species,data=iris)
model$residuals # 잔차 = 관측치 - 예측치 
model$fitted.values

names(model)
summary(model) # <2e-16 *** 

# 사후검정
TukeyHSD(model)


# 1. 파일 가져오기
data <- read.csv("c:/NCS/Rwork/Part-III/three_sample.csv", header=TRUE)

# 2. 데이터 정제/전처리 - NA, outline 제거
data <- subset(data, !is.na(score), c(method, score)) 
data # method, score

# (1) 차트이용 - ontlier 보기(데이터 분포 현황 분석)
plot(data$score) # 차트로 outlier 확인 : 50이상과 음수값
barplot(data$score) # 바 차트
mean(data$score) # 14.45

# (2) outlier 제거 - 평균(14) 이상 제거
length(data$score)#91
data2 <- subset(data, score <= 14) # 14이상 제거
length(data2$score) #88(3개 제거)

# (3) 정제된 데이터 보기 
x <- data2$score
boxplot(x)
plot(x)

# 3. 세집단 subset 작성

# method: 1:방법1, 2:방법2, 3:방법3
data2$method2[data2$method==1] <- "방법1" 
data2$method2[data2$method==2] <- "방법2"
data2$method2[data2$method==3] <- "방법3"

table(data2$method2) # 교육방법 별 빈도수 

# 4. 동질성 검정 - 정규성 검정
# bartlett.test(종속변수 ~ 독립변수) # 독립변수(세 집단)
bartlett.test(score ~ method2, data=data2)

# 귀무가설 : 세 집단 간 분포의 모양이 동질적이다.
# 해설 : 유의수준 0.05보다 크기 때문에 귀무가설을 기각할 수 없다. 

# 동질한 경우 : aov() - Analysis of Variance(분산분석)
# 동질하지 않은 경우 - kruskal.test()

# 5. 분산검정(집단이 3개인 경우 분산분석이라고 함)
# aov(종속변수 ~ 독립변수, data=data set)

# 귀무가설 : 세집단의 평균에 차이가 없다.
result <- aov(score ~ method2, data=data2)

# aov()의 결과값은 summary()함수를 사용해야 p-value 확인 
summary(result) 


