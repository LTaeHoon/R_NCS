          #chap15_2_Regression.R

######################################################
# 회귀분석(Regression Analysis)
######################################################
# - 특정 변수(독립변수:설명변수)가 다른 변수(종속변수:반응변수)에 어떠한 영향을 미치는가 분석

###################################
## 1. 단순회귀분석 
###################################
# - 독립변수와 종속변수가 1개인 경우
# 연구가설 : 제품 적절성은  제품 만족도에 정(正)의 영향을 미친다.
# 연구모델 : 제품적절성(독립변수) -> 제품 만족도(종속변수)

# 단순선형회귀 모델 생성
# 형식) lm(formula= y ~ x 변수, data) # x:독립, y 종속, data=data.frame

product <- read.csv("C:/NCS/Rwork/Part-IV/product.csv", header=TRUE)
head(product) # 친밀도 적절성 만족도(등간척도 - 5점 척도)

str(product) # 'data.frame':  264 obs. of  3 variables:
y = product$제품_만족도 # 종속변수
x = product$제품_적절성 # 독립변수
df <- data.frame(x, y)
df
# 회귀모델 생성 
result.lm <- lm(formula=y ~ x, data=df)
result.lm # 회귀계수 
# (Intercept)   x  
# 0.7789(b:상수)       0.7393 (a:기울기)
# 회귀방정식(1차방정식)
head(df,1) #x=4,y=3
# y = ax+b
x<-5
y = 0.7393*x + 0.7789
y # 3.7361
#오처(잔차:error)
3-3.7361 #-0.7361

names(result.lm) # "coefficients"  "residuals" "fitted.values"
result.lm$residuals[1] #-0.735963 (관측치와 예측치의 차)
result.lm$fitted.values[1] #3.735963 (예측치)


# 모델에 의해서 예측한 Y값 
fitted.values(result.lm)[1:2] # 3.735963 2.996687

# 회귀모델 예측 : 친밀도(설명변수) 5인 경우 -> 만족도(반응변수) 예측
# 형식) predict(model, 설명변수)
predict(result.lm, data.frame(x=5) ) # 3.921361 
predict(result.lm, data.frame(x=1) ) # 2.325461 


# (2) 선형회귀 분석 결과 보기
summary(result.lm)

# <회귀모델 분석순서>
# 1. F검정 통계량 : 모델이 통계적 유의성 검정(p<0.05:유의미하다.) 
# 2. 모델의 설명력 : 0 < R^2 < 1
#   -> 1에 가까울 수록 설명 잘함 
# 3. x변수의 통계적 유의성 검정 

sqrt(0.5865) # 0.7658329
0.7658329**2 # 0.5865
# (3) 단순선형회귀 시각화
# x,y 산점도 그리기 
plot(formula=y ~ x, data=df)
# 회귀분석
result.lm <- lm(formula=y ~ x, data=df)
# 회귀선 
abline(result.lm, col='red')

# 회귀선 : 점과 점 사이의 중앙을 통과하는 직선으로 회귀방정식에 의해서 구한다. (최소자승법)
# 회귀선 : x변수를 이용하여 y변수를 예측하기 위해서 y를 x에 회귀시킨다.
# 회귀 : error가 최소가 되도록 값을 평균으로 좁혀간다는 의미(평균으로 돌아감)  

# y -> x로 회귀(적합)시킨다.
x <- c(1:4)
y <- c(2,1,2,5)
x;y
# [1] 1 2 3 4
# [1] 2 1 2 5
df <- data.frame(x,y)
plot(x,y)

model<-lm(y~x,data=df)
abline(model, col='blue')

model$residuals #예측치 잔차 
# 1  2  3  4 <- index 
# 1 -1 -1  1 <- 잔차 


###################################
## 2. 다중회귀분석
###################################
# - 여러 개의 독립변수 -> 종속변수에 미치는 영향 분석
# 연구가설 : 음료수 제품의 적절성(x1)과 친밀도(x2)는 제품 만족도(y)에 정의 영향을 미친다.
# 연구모델 : 제품 적절성(x1), 제품 친밀도(x2) -> 제품 만족도(y)

product <- read.csv("C:/NCS/Rwork/Part-IV/product.csv", header=TRUE)
head(product) # 친밀도 적절성 만족도(등간척도 - 5점 척도)


#(1) 적절성 + 친밀도 -> 만족도  
y = product$제품_만족도 # 종속변수
x1 = product$제품_친밀도 # 독립변수2
x2 = product$제품_적절성 # 독립변수1

df <- data.frame(x1, x2, y)

result.lm <- lm(formula=y ~ x1 + x2, data=df)
#result.lm <- lm(formula=y ~ ., data=df) # y를 제외한 모든 변수 사용 시 .(점) 사용 

# 계수 확인 
result.lm
# 0.66731(y절편)      0.09593(x1)  0.68522(x2)      

# 회귀방정식 : Y(종속변수) = 상수 + 베타1.x1 + 베타2.x2...
# Y = a + b*x1 + b*x2
head(product, 1) # 3(x1)      4(x2)      3(y)
Y = 0.66731 + 0.09593 * 3 + 0.68522 * 4  
Y # 3.69598

summary(result.lm)
# <회귀모델 분석순서>
# 1. 모델의 유의성 검정 
# 2. 모델의 설명력 = 상관계수^2
# 3. x변수의 유의성 검정 

# 분산팽창요인 
#install.packages("car") # vif() 함수 제공 패키지 설치
library(car) # 메모리 로딩

# 독립 변수간 상관 관계가 높으면 둘 중 하나는 제거해야 함 
#       x1       x2 
# 1.331929 1.331929
# 10 이하이면 상관관계가 없다.
#단계 2 : 분산팽창요인(VIF)
vif(result.lm)
sqrt(vif(result.lm)) 
# 1.154092 1.154092  # 2보다 크면 상관이 있음 

###################################
# 3. 다중공선성(Multicolinearity)
###################################
# - 독립변수 간의 강한 상관관계로 인해서 회귀분석의 결과를 신뢰할 수 없는 현상
# - 생년월일과 나이를 독립변수로 갖는 경우
# - 해결방안 : 강한 상관관계를 갖는 독립변수 제거

# (1) 다중공선성 문제 확인
library(car)
fit <- lm(formula=Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width, data=iris)
vif(fit)
# Sepal.Width Petal.Length  Petal.Width 
# 1.270815    15.097572    14.234335 
# Petal.Length, Petal.Width 는 상관관계가 높다. 둘 중 하나는 독립변수에서 제거 
sqrt(vif(fit))>2 # root(VIF)가 2 이상인 것은 다중공선성 문제 의심 

# (2) iris 변수 간의 상관계수 구하기
cor(iris[,-5]) # 변수간의 상관계수 보기(Species 제외) 
#x변수 들끼 계수값이 높을 수도 있다. -> 해당 변수 제거(모형 수정) <- Petal.Width

# (3) 학습데이터와 검정데이터 분류
x <- sample(1:nrow(iris), 0.7*nrow(iris)) # 전체중 70%만 추출
train <- iris[x, ] # 학습데이터 추출
test <- iris[-x, ] # 검정데이터 추출

# (4) Petal.Width 변수를 제거한 후 회귀분석 
result.lm <- lm(formula=Sepal.Length ~ Sepal.Width + Petal.Length, data=train)
result.lm
summary(result.lm)

# (5) 새로운 값 예측 - predict()함수
# - 회귀분석 결과를 대상으로 회귀방정식을 적용한 새로운 값 예측(Y값)
# 형식) predict(model, test) test에 x변수(회귀분석결과) 값 존재해야함
pred <- predict(result.lm, test)# x변수만 test에서 찾아서 값 예측
pred # test 데이터 셋의 y 예측치(회귀방정식 적용) 
test$Sepal.Length # test 데이터 셋의 y 관측치  
length(pred) # 45개 벡터

plot(pred, col='red', pch=4)
points(test$Sepal.Length, col='blue',pch=3)

# (6) 모델 평가
cor(pred,test$Sepal.Length) # 0.9392315 

summary(pred); summary(test$Sepal.Length)

##########################################
##  4. 선형회귀분석 잔차검정과 모형진단
##########################################

# 1. 변수 모델링  
# 2. 회귀모델 생성 
# 3. 모형의 잔차검정 
#   1) 독립성과 등분산성 검정
#   2) 잔차의 정규성 검정 
#   3) 잔차의 자기상관 검정 
# 4. 다중공선성 검사 
# 5. 회귀모델 생성/ 평가 


names(iris)

# 1. 변수 모델링 : y:Sepal.Length <- x:Sepal.Width,Petal.Length,Petal.Width
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width


# 2. 회귀모델 생성 
model <- lm(formula = formula,  data=iris)
model
attributes(model)

step(model, direction="both")
step(model, direction="forward")
step(model, direction="backward")


# 3. 모형의 잔차검정
methods(plot)
plot(model)
#Hit <Return> to see next plot: 잔차 vs 적합값 -> 패턴없이 무작위 분포(포물선 분포 좋지않은 적합) 
#Hit <Return> to see next plot: Normal Q-Q -> 정규분포 : 대각선이면 잔차의 정규성 
#Hit <Return> to see next plot: 척도 vs 위치 -> 중심을 기준으로 고루 분포 
#Hit <Return> to see next plot: 잔차 vs 지렛대값 -> 중심을 기준으로 고루 분포 

# (1) 등분산성 검정 
plot(model, which =  1)  # 위 4가지 이미지 중 첫번째 which=1
methods('plot') # plot()에서 제공되는 객체 보기 

# (2) 잔차 정규성 검정
attributes(model) # coefficients(계수), residuals(잔차), fitted.values(적합값)
res <- residuals(model) # 잔차 추출 
res2 <- model$residuals
shapiro.test(res) #잔차 정규성 검정 - p-value = 0.9349 >= 0.05
# 귀무가설 : 정규성과 차이가 없다.

# (3) 정규성 시각화  
hist(res, freq = F) 
qqnorm(res)

# (4) 잔차의 자기상관 검정(Durbin-Watson 검정) 
#install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값(통상 1~3 사이)

#alternative hypothesis: true autocorrelation is greater than 0
# [해설] p < 0.05이면 잔차의 자기상관이 존재한다는 의미이다. 
# 유의미한 자기상관이 없다고 할 수 있다. (p-value = 0.6013 >0.05 귀무가설  채택 잔차 간 자기 상관성이 없다.)


# 4. 다중공선성 검사 
library(car)
sqrt(vif(model)) > 2 # TRUE 

# 5. 모델 생성/평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length 
model <- lm(formula = formula,  data=iris)
summary(model) # 모델 평가

