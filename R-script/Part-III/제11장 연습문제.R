#################################
## <제11장 연습문제>
################################# 

#01. MASS 패키지에 있는 Animals 데이터 셋을 이용하여 각 단계에 맞게 기술통계량을 구하시오.

#<단계 1> MASS 패키지 설치 및 메모리 로딩
library(MASS) # MASS 패키지 불러오기
data(Animals) # Animals 데이터셋 로딩
head(Animals) # Animals 데이터셋 보기

#<단계 2> R의 기본함수를 이용하여 brain 칼럼을 대상으로 다음 기술통계량 구하기
dim(Animals)# Animals 데이터 셋 차원보기
str(Animals)
summary(Animals$brain)
attach(Animals)
mean(brain)
sd(brain)
var(brain)
range(brain)
#<단계 3> 패키지에서 제공되는 describe()과 freq()함수를 이용하여 Animals 데이터 셋 전체를 대상으로 기술통계량 구하기
describe(Animals)
freq(Animals)


#02. descriptive.csv 데이터 셋을 대상으로 다음 조건에 맞게 빈도분석 및 기술통계량 분석을 수행하시오
setwd("c:/NCS/Rwork/Part-III")
data <- read.csv("descriptive.csv", header=TRUE)
head(data) # 데이터셋 확인

# 조건1) 명목척도 변수인 학교유형(type), 합격여부(pass) 변수에 대해 빈도분석을 수행하고 
# 결과를 막대그래프와 파이차트로 시각화 
typedata<- table(data$type)
barplot(typedata)
lab1 <- round(prop.table(typedata)*100,2)
pie(typedata,label=lab1)

passdata <- table(data$pass)
barplot(passdata)
lab <- round(prop.table(passdata)*100,2)
pie(passdata,labels = lab)

# 조건2) 비율척도 변수인 나이 변수에 대해 요약치(평균,표준편차)와 비대칭도(왜도와 첨도)
# 통계량을 구하고, 히스토그램으로 비대칭도 설명
summary(data$age)
hist(data$age)
skewness(data$age) # 왜도도
#[1] 0.3804892 0보다 크므로 오른쪽으로 꼬리가 길고 왼쪽으로 기울어진 형태

kurtosis(data$age)
#[1] 1.866623 정규분포가 3인데 그 값보다 작으므로 정규분포보다 덜 올라와 있다.

hist(data$age, freq=F)
# 나이변수의 확률밀도 함수
lines(density(data$age), col='blue')
# 정규분포
x <- seq(40, 70, 1)
curve( dnorm(x, mean(data$age), sd(data$age)), col='red', add = T)

# 정규분포 검정
# qqplot
qqnorm(data$age,main='age qq-plot')
qqline(data$age,col='red')

# shapiro-test
shapiro.test(data$age)
#W = 0.92312, p-value = 2.633e-11 <0.05 보다 작으므로 대립가설 채택
# 정규분포와 차이가 있다.

# 조건3) 나이 변수에 대한 밀도분포곡선과 정규분포 곡선으로 정규분포 검정
