#################################
## <제15장 연습문제>
################################# 

# 01. product.csv 파일의 데이터를 이용하여 다음과 같은 단계로 다중회귀분석을 수행하시오.
product <- read.csv("C:/Rwork/Part-IV/product.csv", header=TRUE)

#  단계1 : 학습데이터(train),검정데이터(test)를 7 : 3 비율로 샘플링
idx <- sample(1:nrow(product), 0.7*nrow(product))
train <- product[idx,] # result중 70%
dim(train) # [1] 184   3
train # 학습데이터

test <- product[-idx, ] # result중 나머지 30%
dim(test) # [1] 80  3
test # 검정 데이터

#  단계2 : 학습데이터 이용 회귀모델 생성 
#           변수 모델링) y변수 : 제품_만족도, x변수 : 제품_적절성, 제품_친밀도
model <- lm(formula=제품_만족도 ~ 제품_적절성 + 제품_친밀도, data=train)
summary(model) # 학습데이터 분석 -> p-value: < 2.2e-16

#  단계3 : 검정데이터 이용 모델 예측치 생성 
pred <- predict(model, test) # 1) 예측치 생성 

#  단계4 : 모델 평가 : cor()함수 이용  
cor(pred, test$제품_만족도) # 2) 모델 평가

# 02. ggplot2패키지에서 제공하는 diamonds 데이터 셋을 대상으로 
# carat, table, depth 변수 중 다이아몬드의 가격(price)에 영향을 
# 미치는 관계를 다중회귀 분석을 이용하여 예측하시오.
#조건1) 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
#조건2) 다중회귀 분석 결과를 정(+)과 부(-) 관계로 해설

library(ggplot2)
data(diamonds)
# diamonds에서 비율척도 대상으로 식 작성 
formula <- price ~ carat +  table + depth
head(diamonds)
result <- lm(formula, data=diamonds) 
summary(result) # 회귀분석 결과

# <해설>carat은 price에 정(+)의 영향을 미치지만, table과 depth는 부(-)의 영향을 미친다.


# 03. mpg 데이터 셋을 대상으로 7:3 비율로 학습데이터와 검정데이터로 각각 
# 샘플링한 후 각 단계별로 분류분석을 수행하시오.

# 조건) 변수모델링 : 독립변수(설명변수) : displ + cyl + year 
#       종속변수(반응변수) : cty

library(party)
library(ggplot2)
data(mpg)
str(mpg) 

# 단계1 : 학습데이터와 검정데이터 샘플링
idx <- sample(1:nrow(mpg), nrow(mpg)*0.7)
# 학습데이터
train <- mpg[idx,] # train -> t가 1인 레코드
dim(train) # [1] 163  11
# 검정데이터
test <- mpg[-idx,]  # test -> t가 2인 레코드
dim(test) # [1]  71 11

# 단계2 : formula(공식) 생성
f <- cty ~ displ + cyl + year

# 단계3 : 학습데이터 이용 분류모델 생성 
train_model <- ctree(formula = f, data = train)

# 단계4 : 검정데이터 이용 예측치 생성 및 평가 
pred <- predict(train_model, test)
cor(pred, test$cty) # 0.8675849

# 단계5 : 분류분석 결과 시각화
plot(train_model)

# 단계6 : 분류분석 결과 해설
# <해설> 실린더가 5이하이면 엔진크기에 의해서 23개가 분류되고, 
# 실린더가 5이상이고,  6이하이면 27개가 분류되고, 6을 초과한 경우
# 21개가 분류된다.


# 04. weather 데이터를 다음과 같은 단계별로 분류분석을 수행하시오. 

# 조건1) rpart() 함수 이용 분류모델 생성 
# 조건2) 변수 모델링 : y변수 : RainTomorrow, x변수 : Date와 RainToday 변수 제외한 나머지 변수
# 조건3) 비가 올 확률이 50% 이상이면 ‘Yes Rain’, 50% 미만이면 ‘No Rain’으로 범주화

# 단계1 : 데이터 가져오기
library(rpart)
weather = read.csv("c:/Rwork/Part-IV/weather.csv", header=TRUE) 

# 단계2 : 데이터 샘플링
weather.df <- weather[, c(-1,-14)]
nrow(weather.df)
idx <- sample(1:nrow(weather.df), nrow(weather.df)*0.7)
weather_train <- weather.df[idx, ]
weather_test <- weather.df[-idx, ]

# 단계3 : 분류모델 생성
weather_model <- rpart(RainTomorrow ~ ., data = weather.df)
weather_model # Humidity 중요변수 

# 단계4 : 예측치 생성 : 검정데이터 이용 
weater_pred <- predict(weather_model, weather_test)
weater_pred

## [내용 추가] rpart 모델 시각화 도구 
install.packages('rpart.plot')
library(rpart.plot)

X11()
rpart.plot(weather_model)


# 단계5 : 예측 확률 범주화('Yes Rain', 'No Rain') 
weater_class <- ifelse(weater_pred[,1] >=0.5, 'No Rain', 'Rain')

# 단계6 : 혼돈 matrix 생성 및 분류 정확도 구하기
table(weater_class, weather_test$RainTomorrow)
#weater_class No Yes
#    No Rain 83   6
#    Rain     2  19
(83 + 19) / nrow(weather_test) # 0.9272727
