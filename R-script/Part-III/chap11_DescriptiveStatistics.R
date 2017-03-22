##chap11_DescriptiveStatistics

########################################
## chap11_Descriptive Statistics 수업내용
########################################

# 기술통계(Descriptive Statistics) 
# 대푯값 : 평균(Mean), 합계(Sum), 중위수(Median), 최빈수(mode), 사분위수(quartile) 등
# 산포도 : 분산(Variance), 표준편차(Standard deviation), 최소값(Minimum), 최대값(Maximum), 범위(Range) 등 
# 비대칭도 : 왜도(Skewness)- 얼마나 기울어졌냐, 첨도(Kurtosis) - 얼마나 뾰쪽한가


# - 실습파일 가져오기
getwd()
setwd("c:/NCS/Rwork/Part-III")
data <- read.csv("descriptive.csv", header=TRUE)
head(data) # 데이터셋 확인

# 인구통계학변수 : 성장하면서 갖추어지는 변수


# 1. 척도별 기술통계량
#  - 데이터 특성 보기(전체 데이터 대상)
dim(data) # 행(300)과 열(8) 정보 - 차원보기
length(data) # 열(8) 길이
length(data$survey) #survey 컬럼의 관찰치 - 행(300) 

str(data) # 데이터 구조보기 -> 데이터 종류,행/열,data
str(data$survey) 

# 데이터 특성(최소,최대,평균,분위수,노이즈-NA) 제공
summary(data) 

# 1) 명목척도 변수의 기술통계량
# - 명목상 의미없는 수치로 표현된 변수 - 성별(gender)     
length(data$gender)
summary(data$gender) # 최소,최대,중위수,평균-의미없음 , 명목척도에 대해 summary 적용은 의미없음
table(data$gender) # 각 성별 빈도수 - outlier 확인-> 0, 5

data <- subset(data,data$gender == 1 | data$gender == 2) # 성별 outlier 제거
x <- table(data$gender) # 성별에 대한 빈도수 저장
x # outlier 제거 확인
barplot(x) # 범주형(명목/서열척도) 시각화 -> 막대차트

prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)

# 2) 서열척도 변수의 기술통계량
# - 계급순위를 수치로 표현한 변수 - 학력수준(level)
length(data$level) # 학력수준 - 서열
summary(data$level) # 명목척도와 함께 의미없음
table(data$level)  # 빈도분석 - 의미있음

x1 <- table(data$level) # 각 학력수준에 빈도수 저장
x1
barplot(x1) # 명목/서열척도 -> 막대차트

# 3) 등간척도 변수의 기술통계량
# - 속성의 간격이 일정한 변수(survey) - 덧셈/뺄셈 연산 가능
survey <- data$survey
survey

summary(survey) # 만족도(5점 척도)인 경우 의미 있음 -> 2.6(평균이상)
x1<-table(survey) # 빈도수
x1
#1  2  3  4  5 
#20 72 61 25  7

hist(survey) # 등간척도 시각화 -> 히스토그림
pie(x1)

# 4) 비율척도 변수의 기술통계량
# - 수치로 직접 입력한 변수(cost)  
length(data$cost)
summary(data$cost) # 요약통계량 - 의미있음(mean) - 8.784
mean(data$cost) # NA
data$cost

# 데이터 정제 - 결측치 제거 및 outlier 제거
plot(data$cost)
data <- subset(data,data$cost >= 2 & data$cost <= 10) # 총점기준
data
x <- data$cost
plot(x)

##### cost 변수의 대푯값 ####
mean(x) # 평균 : 5.354
# 평균이 극단치에 영향을 받는 경우 - 중위수(median) 대체
median(x) # 5.4  
min(x)
max(x)
range(x) # min ~ max
sort(x) # 오름차순 
sort(x, decreasing=T) # 내림차순  

#### xx의 최빈수 구하기
xx <- c(3,6,1,3,6,4,3,6)

# 최빈수
tb<-table(xx)
tb
# 1 3 4 6 <-xx 변량
# 1 3 1 3 <-빈도수
max <- max(tb)
max #3

which(tb==3) #index
# 3 6 <-최빈수 
# 2 4 <-index

# x(cost) 변수의 최빈수
x
tbx <- table(x)
max<-max(tbx)
which(tbx==18)
# 5  <-최빈수
# 19 <-index
tbx[19]
#### cost 변수의 산포도 #####
var(x) # 분산
sd(x) # 표준편차는 분산의 양의 제곱근

#표준편차 -> 분산
sd(x)**2

quantile(x, 1/4) # 1 사분위수 - 25%, 4.6
quantile(x, 3/4) # 3 사분위수 - 75%, 6.2

min(x) # 최소값
max(x) # 최대값
range(x) # 범위(min ~ max)
#########################


# 5) 패키지를 이용한 비대칭도 구하기 
install.packages("moments")  # 왜도/첨도 위한 패키지 설치   
library(moments)
cost <- data$cost # 정제된 data
cost
# 왜도 - 평균을 중심으로 기울어진 정도
skewness(cost) # -0.2974908 
# 0 보다 작으면 왼쪽방향 비대칭 꼬리 왼쪽으로 길게 꼬리가 만들어져 오른쪽으로 기울어짐
# 0 보다 크면 오른쪽 방향 비대칭 꼬리  오른쪽으로 길게 꼬리가 만들어져 왼쪽으로 기울어짐

#첨도 - 표준정규분포와 비교하여 얼마나 뽀족한가 측정 지표
kurtosis(cost) # 2.683438     

# 기본 히스토그램 
hist(cost)

# 변인이 정규분포를 가지고 있으면 모수검정 아니면 비모수 검정 통계 사용

# 1. 히스토그램 확률밀도/표준정규분포 곡선 
hist(cost, freq = F)
# 확률밀도 분포 곡선 
lines(density(cost), col='blue')
# 표준정규분포 곡선 
x <- seq(0, 8, 0.1)
curve( dnorm(x, mean(cost), sd(cost)), col='red', add = T)

# 2. QQ-plot (변인 정규 분포 확인)
qqnorm(cost,main='cost qq-plot')
qqline(cost,col='red')

# 정규성 검정 (정규 분포 검정 방법)
# 귀무가설 : 정규 분포와 차이가 없다. -> p-value >= 유의수준(알파)-0.05
# 대립가설 : 정규 분포와 차이가 있다. -> p-value < 유의수준(알파)-0.05

shapiro.test(cost)
# W = 0.98187, p-value = 0.002959 <0.05
# 귀무가설 기각 , 대립가설 채택
# cost는 정규 분포를 띄지 않는다.

# cost 비율척도 변수의 기술통계

# 변수 접근 시 컬럼명만 사용하여 접근하고 싶을때 attach 사용

attach(data) #data를 붙여라! -> data$ 생략     
length(cost)
summary(cost) # 요약통계량 - 의미있음(mean)
mean(cost) # 가장 의미있음
min(cost)
max(cost)
range(cost) # min ~ max
sd<- sd(cost, na.rm=T)
var(cost, na.rm=T)
sqrt(var(cost, na.rm=T))
sd(cost, na.rm=T)

shapiro.test(cost)

sort(cost) # 오름차순 
sort(cost, decreasing=T) # 내림차순
detach(data) # attach(data) 해제

# NA가 있으면 error 발생 함수
test <-c(1:5,NA,10:20)
min(test) # NA 출력
max(test) # NA 출력
range(test) # NA 출력
mean(test) # NA 출력     
# 결측치 데이터 제거 후 통계량 구함
min(test, na.rm=T) # na.rm=T 결측데이터 제거 방법1
max(test, na.rm=T)
range(test, na.rm=T) 
mean(test, na.rm=T)

#  2. 패키지 이용 기술통계량 구하기

########################<기술통계 패키지1>#########################
# 패키지 : Hmisc -> 기능을 구현해 놓은 패키지
install.packages("Hmisc") # 웹사이트 접속하여 패키지 설치
library(Hmisc) # 해당 패키지를 메모리 로딩

# 전체 변수 대상 기술통계량 제공 - 빈도와 비율 데이터 일괄 수행
describe(data) # Hmisc 패키지에서 제공되는 함수
# 명목,서열,등간척도 - n, missing,unique, 빈도수,비율
# 비율척도 - n, missing, unique, mean, lowest, highest

# 개별 변수 기술통계량
describe(data$gender) # 특정 변수(명목) 기술통계량 - 비율 제공
describe(data$age) # 특정 변수(비율) 기술통계량 - lowest, highest

summary(data$age)
################################################################

########################<기술통계 패키지2>#########################
# 기술통계패키지(Hmisc 보다 유용)
install.packages("prettyR")
library(prettyR)

# 전체 변수 대상      
freq(data) # 각 변수별 : 빈도, 결측치, 백분율, 특징-소수점 제공
# 개별 변수 대상
freq(data$gender) # 빈도와 비율 제공
################################################################


#  3. 기술통계량 보고서 작성법

# 1) 거주지역 
data$resident2[data$resident == 1] <-"특별시"
data$resident2[data$resident >=2 & data$resident <=4] <-"광역시"
data$resident2[data$resident == 5] <-"시구군"

x<- table(data$resident2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#광역시 시구군 특별시 
#37.66  14.72  47.62

# 2) 성별
data$gender2[data$gender== 1] <-"남자"
data$gender2[data$gender== 2] <-"여자"
x<- table(data$gender2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#남자  여자 
#58.87 41.13

# 3) 나이
summary(data$age)# 40 ~ 69
data$age2[data$age <= 45] <-"중년층"
data$age2[data$age >=46 & data$age <=59] <-"장년층"
data$age2[data$age >= 60] <-"노년층"
x<- table(data$age2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#노년층 장년층 중년층 
#24.60  68.15   7.26

# 4) 학력수준
data$level2[data$level== 1] <-"고졸"
data$level2[data$level== 2] <-"대졸"
data$level2[data$level== 3] <-"대학원졸"

x<- table(data$level2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#고졸     대졸 대학원졸 
#39.41    36.44    24.15

# 5) 합격여부
data$pass2[data$pass== 1] <-"합격"
data$pass2[data$pass== 2] <-"실패"
x<- table(data$pass2)
x
prop.table(x) # 비율 계산 : 0< x <1 사이의 값
y <-  prop.table(x)
round(y*100, 2) #백분율 적용(소수점 2자리)
#실패  합격 
#40.85 59.15

head(data)
# data Mart
--------------------------------------------------------------------------------
  # resident   gender      age	  level	     cost	   type    	 survey	   pass
  # 거주지역   성별       나이  학력수준     생활비  학교유형  만족도    합격여부
  # 명목(1~3)  명목(1,2)  비율  서열(1,2,3)  비율    명목(1,2) 등간(5점) 명목(1,2)
  ---------------------------------------------------------------------------------  
  
  # <논문/보고서 에서 표본의 인구통계적특성 결과 제시 방법>
  
  length(data$cost) # 248 
describe(data)
summary(data$cost) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.100   4.600   5.400   5.361   6.200   7.900 
sum(data$cost) # 1345.5

length(data$survey) # 248
describe(data)
summary(data$survey) 
sum(data$survey, na.rm=T) # 424

# 기술통계량 정제 데이터 저장 
getwd()
setwd("c:/NCS/Rwork/Part-III")

write.csv(data,"cleanDescriptive.csv", quote=F, row.names=F) # 행 이름 제거
