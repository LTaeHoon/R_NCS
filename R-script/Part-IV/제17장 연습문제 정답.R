#################################
## <제17장 연습문제>
################################# 

#----------------------------------------------------
# 01. 시계열 데이터를 대상으로 다음과 같은 조건으로 추세선을 시각화 하시오.

#<데이터 셋 준비>
data(EuStockMarkets)
head(EuStockMarkets)

EuStock<- data.frame(EuStockMarkets)
head(EuStock)

# <조건1> Second 벡터(1~500값)와 DAX 벡터(DAX 컬럼의 1001~1500 벡터값) 생성
Second <- c(1:500) # 초단 단위로 벡터 생성 
DAX <- EuStock$DAX[1001:1500] # DAX 칼럼으로 백터 생성 

# <조건2> Second와 DAX 벡터 이용 데이터프레임(EuStock.df) 생성
EuStock.df <- data.frame(Second, DAX)
EuStock.df

# <조건3> ggplot() 이용  X축(Second), Y축(DAX) 형태로 시계열 데이터를 점과 선으로 시각화
library(ggplot2)

# 선색 : green,  점색 : orange
ggplot(EuStock.df, aes(x=Second, y=DAX)) + geom_point(color="orange")
ggplot(EuStock.df, aes(x=Second, y=DAX))+geom_point(color="orange")+ geom_line(color="green")

# <조건4> qplot()함수를 이용하여 추세선과 표준오차 그리기 
qplot(Second, DAX, data=EuStock.df, geom=c("point", "smooth")) # method 생략 가능 
qplot(Second, DAX, data=EuStock.df, geom=c("point", "smooth"), method="lm") 


#02. 시계열 데이터를 대상으로 다음과 같은 단계별로 시계열 모형을 생성하고, 예측하시오.

# <데이터 셋 준비>
#  <문제1>에서 생성된 EuStock.df 

#단계1 : 시계열 자료 생성 : DAX 칼럼을 대상으로 2017년1월 기준 시계열 자료 생성 
tsdata <- ts(EuStock.df$DAX, start=c(2017, 1), frequency=12)
tsdata

#단계2 : 시계열 자료 분해 : 
# (1) stl()함수 이용 분해 시각화
plot(stl(tsdata, "periodic")) # 주기적인 

# (2) decompose()함수 이용 분해 시각화, 불규칙요인만 시각화 
m <- decompose(tsdata)
plot(m) # 3개 요인 포함 

# (3) 계절요인, 추세요인 제거 그래프-불규칙요인만 출력
plot(tsdata - m$seasonal - m$trend) 

# 단계3 : ARIMA 시계열 모형 생성 
library(forecast) 

model <- auto.arima(tsdata) # 자동으로 최적의 ARIMA 모형 제공
model

# 단계4 : 시계열 예측 : 향후 3년, 90%, 95% 신뢰수준으로 예측 및 시각화
fore <- forecast(model, level=c(90, 95), h=36) # 향후 36개월(3년) 예측 
fore
plot(fore)
