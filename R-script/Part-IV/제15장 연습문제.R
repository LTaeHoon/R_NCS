#################################
## <제15장 연습문제>
################################# 

# 01. product.csv 파일의 데이터를 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.
product <- read.csv("C:/NCS/Rwork/Part-IV/product.csv", header=TRUE)

#  단계1 : 학습데이터(train),검정데이터(test)를 7 : 3 비율로 샘플링
?sample
#  단계2 : 학습데이터 이용 회귀모델 생성 
#           변수 모델링) y변수 : 제품_만족도, x변수 : 제품_적절성, 제품_친밀도

#  단계3 : 검정데이터 이용 모델 예측치 생성 

#  단계4 : 모델 평가 : cor()함수 이용  


# 02. ggplot2패키지에서 제공하는 diamonds 데이터 셋을 대상으로 
# carat, table, depth 변수 중 다이아몬드의 가격(price)에 영향을 
# 미치는 관계를 다중회귀 분석을 이용하여 예측하시오.
#조건1) 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
#조건2) 다중회귀 분석 결과를 정(+)과 부(-) 관계로 해설

library(ggplot2)
data(diamonds)

str(diamonds)

idx<-sample(1:nrow(diamonds),0.7*nrow(diamonds))
length(idx)
train<- diamonds[idx,]
train
test<-diamonds[-idx,]
test
length(test)
model <- lm(formula = price~carat+table+depth,data=train)
library(car)
vif(model) # 다중 공산성 세 독립변수의 상관관계가 높지 않다.

summary(model)
model

pred_exam <- predict(model,test)
pred_exam

cor(pred_exam,test$price) #[1] 0.9244022

# 모델 평가
plot(model)
res<-model$residuals
length(model$residuals)
shapiro.test(res)
#p-value = 0.9349 >0.05 귀무가설 채택 정규성 분포 

# (3) 정규성 시각화  
hist(res,freq = T) 
x <- seq(from=-1,to= 1.0,by=0.1)
x
curve(dnorm(x,mean=mean(res),sd=sd(res)),-1,1)
qqnorm(res)

# (4) 잔차의 자기상관 검정(Durbin-Watson 검정) 
#install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값(통상 1~3 사이)

#alternative hypothesis: true autocorrelation is greater than 0
# [해설] p < 0.05이면 잔차의 자기상관이 존재한다는 의미이다. 
# 유의미한 자기상관이 없다고 할 수 있다. (p-value = 0.6013 >0.05 귀무가설  채택 잔차 간 자기 상관성이 없다.)


# 03. mpg 데이터 셋을 대상으로 7:3 비율로 학습데이터와 검정데이터로 각각 
# 샘플링한 후 각 단계별로 분류분석을 수행하시오.

# 조건) 변수모델링 : 독립변수(설명변수) : displ + cyl + year 
#       종속변수(반응변수) : cty

library(ggplot2)
data(mpg)
str(mpg) 

# 단계1 : 학습데이터와 검정데이터 샘플링
idx <- sample(1:nrow(mpg),0.7*nrow(mpg))
length(idx)
train <- mpg[idx,]
test <- mpg[-idx,]
# 단계2 : formula(공식) 생성
formula <- cty ~ displ + cyl + year
# 단계3 : 학습데이터에 분류모델 적용
library(party)
model <- ctree(formula,data=train)

# 단계4 : 검정데이터에 분류모델 적용
pred<-predict(model,test)
pred

cor(pred,test$cty) # 0.8794177

# 단계5 : 분류분석 결과 시각화
plot(model)
# 단계6 : 분류분석 결과 해설
#[해설] 실린더 수가 5이하

# 04. weather 데이터를 다음과 같은 단계별로 분류분석을 수행하시오. 

# 조건1) rpart() 함수 이용 분류모델 생성 
# 조건2) 변수 모델링 : y변수 : RainTomorrow, x변수 : Date와 RainToday 변수 제외한 나머지 변수
# 조건3) 비가 올 확률이 50% 이상이면 ‘Yes Rain’, 50% 미만이면 ‘No Rain’으로 범주화

# 단계1 : 데이터 가져오기
library(rpart)
weather = read.csv("c:/NCS/Rwork/Part-IV/weather.csv", header=TRUE) 

# 단계2 : 데이터 샘플링
weather_sub <- weather[,c(-1,-14)]
idx <- sample(1:nrow(weather_sub),0.7*nrow(weather_sub))
train <- weather_sub[idx,]
test <- weather_sub[-idx,]
# 단계3 : 분류모델 생성
formula <- RainTomorrow ~ .
weather_train<-rpart(formula, data=train)
weather_train
X11()
plot(weather_train)
text(weather_train, use.n = T,cex=0.5)
post(weather_train,file='')

# 단계4 : 예측치 생성 : 검정데이터 이용 
pred <- predict(weather_train,test)
pred

# 단계5 : 예측 확률 범주화('Yes Rain', 'No Rain') 
pred2 <- ifelse(pred[,2]>=0.5,'Yes','No')
pred2

# 단계6 : 혼돈 matrix 생성 및 분류 정확도 구하기
re<-table(pred2,test$RainTomorrow)
acc <-(re[1,1]+re[2,2])/length(pred2) 
acc
