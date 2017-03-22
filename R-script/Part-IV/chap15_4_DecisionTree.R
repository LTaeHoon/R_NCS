#chap15_4_DecisionTree

###################################################
# 분류분석(ClassificationAnalysis) = Decision Tree
###################################################
# - 종속변수(y변수) 존재
# - 종속변수 : 예측에 Focus을 두는 변수
# - 비모수 검정 : 선형성, 정규성, 등분산성 가정 필요없음
# - 단점 : 유의수준 판단 기준 없음(추론 기능 없음)
# - 규칙(Rule)을 기반으로 의사결정트리 생성


###########################################
# party 패키지를 적용한 분류분석
###########################################

# part패키지 설치
install.packages("party")
library(party) # ctree() 제공

#----------------<<실습1>>-------------------------------
# R에서 기본으로 제공되는 airquality 데이터 셋을 이용하기 위해서 
#  - Temp(온도)에 영향을 미치는 변수를 알아보기 -  
# airquality 데이터 셋 153개의 관측치와 6개의 변수로 구성되어 있으며
# New York의 대기에 관한 질을 측정한 데이터 셋이다. 
# 주요 변수로는 Ozone(오존수치), Solar.R(태양광), Wind(바람), 
# Temp(온도), Month(측정 월), Day(측정 날짜) 등이 있다.
#--------------------------------------------------------

# airquality 데이터 셋 로딩
library(datasets)
str(airquality)

# formula 생성
formula <-  Temp ~ Solar.R +  Wind + Ozone

# 분류모델 생성 : formula를 이용하여 분류모델 생성 
air_ctree <- ctree(formula, data=airquality)
air_ctree

plot(air_ctree)

result <- subset(airquality,Ozone<=37&Wind>15.5)
summary(result$Temp)

#----------------<<실습2>>-------------------------------
# 학습데이터와 검정데이터 샘플링으로 분류분석 
# 4개변수(Sepal Length,Sepal Width,Petal Length,Petal Width) 
# 값에 따라서 꽃의 종류(Species)가 분류되는 분석 과정
#--------------------------------------------------------

#단계1 : 학습데이터와 검증데이터 샘플링
#set.seed(1234) # 메모리에 시드값 적용 - 동일값 생성 
idx <- sample(1:nrow(iris), nrow(iris) * 0.7) 
train <- iris[idx,] 
test <- iris[-idx,]  

# 단계2 : formula 생성 
#  -> 형식) 변수 <- 종속변수 ~ 독립변수
formula <- Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width 


#단계3 : 학습데이터 이용 분류모델 생성(ctree()함수 이용)
iris_ctree <- ctree(formula, data=train) # 학습데이터로 분류모델(tree) 생성
iris_ctree # Petal.Length,Petal.Width 중요변수

#단계4 : 분류모델 플로팅
# plot() 이용 - 의사결정 트리로 결과 플로팅
plot(iris_ctree, type="simple") 
plot(iris_ctree) # 의사결정트리 해석

result_iris <- subset(train,Petal.Length>1.9&Petal.Width<=1.7&Petal.Length>4.7)
length(result_iris)
summary(result_iris$Species)

#단계5 : 예측치 
pred <- predict(iris_ctree, test) # 45
pred # Y변수의 변수값으로 예측 

#단계6 : 모델 평가 
table(pred, test$Species)
# pred         setosa versicolor virginica
# setosa         17          0         0
# versicolor      0         11         1 
# virginica       0          0        16

(17+11+16)/nrow(test) #0.9777778

#-------------------<<실습3>>-------------------------
# K겹 교차검정 샘플링으로 분류분석 
# - iris 데이터 셋을 대상으로 K=3, R=2 교차검정
#   샘플링 방법으로 분류분석 
#-----------------------------------------------------

#단계1: K겹 교차검정을 위한 샘플링 
library(cvTools)
cross <- cvFolds(nrow(iris), K=3, R=2) 

#단계2: K겹 교차검정 데이터 보기
str(cross) # 구조 보기 

cross # 5겹 교차검정 데이터 보기
length(cross$which) # 150
dim(cross$subsets) # 150   2


#단계3: K겹 교차검정 수행  
R=1:2 # 2회 반복  
K=1:3 # 3겹  
CNT=0 # 카운터 변수 -> 1차 테스트 
ACC <- numeric() # 분류정확도 저장 -> 2차 모델 생성  

for(r in R){ # 2회 
  cat('\n R=',r, '\n')  
  
  for(k in K){ # 3회 
    datas_idx <- cross$subsets[cross$which==k, r]  
    #cat('K=',k,'검정데이터 \n')  
    #print(iris[datas_idx, ])  # 검정데이터 생성
    test <- iris[datas_idx, ]
    
    for(i in K[-k]){  # 6회(3*2) 반복
      formula <- Species ~ .
      datas_idx <- cross$subsets[cross$which==i, r]  
     # cat('K=',i,'훈련데이터 \n') # 학습데이터 생성 
     # print(iris[datas_idx, ])
      train <- iris[datas_idx, ]
      model <- ctree(formula,data=train) # 모델 생성
      pred <- predict(model,test) # 예측치 생성
      res <- table(pred,test$Species) # 분류 정확도(acc)
      CNT <- CNT + 1 
      ACC[CNT] <- (res[1,1] + res[2,2] + res[3,3])/nrow(test)
    }
    
  } # outer for K   
} # outer for R

CNT # 12
ACC
#0.92 0.86 0.94 0.96 0.94 0.94 0.90 1.00 0.84 0.90 0.86 0.92
length(ACC)
#전체 분류정확도를 산술평균으로 계산 
mean(ACC)
# 0.915 

#      Test     Train
#  K=1 : D1   -> D2, D3 
#  K=2 : D2   -> D1, D2
#  K=3 : D3   -> D1, D2 -> 6회 
#  K=1 : D1   -> D2, D3 
#  K=2 : D2   -> D1, D2
#  K=3 : D3   -> D1, D2 -> 12회 



#----------------<<실습4>>-------------------------------
# AdultUCI 데이터 셋을 이용한 분류분석
# AdultUCI 데이터 셋에 관한 설명
# arules패키지에서 제공되는 데이터 셋으로 성인을 대상으로 
# 인구 소득에 관한 설문조사 데이터를 포함하고 있다. 
# 전체 48,842개의 관측치와 15개 변수로 구성되어 있다. 
# 주요 변수 : age(나이),workclass(직업:4개),education(교육수준:16개),
# occupation(직업:12개),race(인종:아시아계,백인),capital-gain(자본이득),
# capital-loss(자본손실),hours-per-week(주당 근무시간),
# native-country(국가),income(소득)
#-------------------------------------------------------------
install.packages("arules") # AdultUCI 데이터 셋 이용을 위한 패키지 설치
library(arules)
data("AdultUCI")
str(AdultUCI) # 'data.frame':	48842 obs. of  15 variables:
names(AdultUCI)

# 데이터 샘플링 - 10,000개 관측치 선택 
set.seed(1234) # 메모리에 시드 값 적용
choice <- sample(1:nrow(AdultUCI), 10000)
choice
adult.df <-  AdultUCI[choice, ]  
str(adult.df) # ' # 'data.frame':	10000 obs. of  15 variables:

# 변수 추출 및 데이터 프레임 생성
# (1) 변수 추출
capital<- adult.df$`capital-gain`
hours<- adult.df$`hours-per-week`
education <- adult.df$`education-num`
race <- adult.df$race
age <- adult.df$age
income <- adult.df$income

# (2) 데이터프레임 생성
adult_df <- data.frame(capital=capital, age=age, race=race, hours=hours, education=education, income=income)
str(adult_df) # 'data.frame':	10000 obs. of  6 variables:

# formula 생성 - 자본이득(capital)에 영향을 미치는 변수 
formula <-  capital ~ income + education + hours + race + age

# ctree() : 분류모델 생성 및 예측
adult_ctree <- ctree(formula, data=adult_df)
adult_ctree # 가장 큰 영향을 미치는 변수 - income, education

# 분류모델 플로팅
plot(adult_ctree)

# <해설> 자본이득(capital)에 가장 큰 영향을 미치는 변수는 income이고, 
# 두 번째는 education 변수이다. 



##############################
## ctree() vs rpart()
#########################

# y변수가 범주형인 경우
# ctree() : y변수의 범주로 예측 -> setosa versicolor virginica
# rpart() : y변수의 비율 예측  -> 0.85    0.001      0.13014


#단계1 : 학습데이터와 검증데이터 샘플링
#set.seed(1234) # 메모리에 시드값 적용 - 동일값 생성 
idx <- sample(1:nrow(iris), nrow(iris) * 0.7) 
train <- iris[idx,] 
test <- iris[-idx,]  

# 단계2 : formula 생성 
#  -> 형식) 변수 <- 종속변수 ~ 독립변수
formula <- Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width 


#단계3 : 학습데이터 이용 분류모델 생성(ctree()함수 이용)
iris_ctree <- ctree(formula, data=train) # 학습데이터로 분류모델(tree) 생성
iris_rpart <- rpart(formula, data=train) 

#단계 4 : 예측치 생성 (검정데이터)
pred <- predict(iris_ctree, test)
table(pred, test$Species)

pred2 <- predict(iris_rpart, test)
pred2
str(pred2)

cpred2 <- ifelse(pred2[,1]>=0.5,'setosa',ifelse(pred2[,2]>=0.5,'versicolor','virginica'))
head(cpred2)
table(cpred2, test$Species)

#################################################
# rpart 패키지 적용 분류분석
#################################################

# 1. rpart()함수 간단 실습 
install.packages("rpart")
library(rpart)

X11() # 별도창 
formula <- Species ~ .
iris.df <- rpart(formula, data=iris)
iris.df  
plot(iris.df ) # 트리 프레임 보임
text(iris.df, use.n=T, cex=0.6) # 텍스트 추가
post(iris.df, file="")
# <해석>
# iris의 Species(꽃의 종류)변수를 분류하는 가장 중요한 변수는 
# Petal.Length와 Petal.Width로 나타난다. 


# 2. rpart() 응용 실습
#  weather.csv를 weather로 읽어서 RainTomorrow가 y변수, Data, RainTody를
#  제외한 나머지 변수가 x변수가 되도록 하여 decision tree를 작성

########################## <weather set> ###########################
# Date(측정날짜) MinTemp(최저기온) MaxTemp(최대기온) Rainfall(강수량) 
# Sunshine(햇빛)  WindGustDir(돌풍 방향) WindGustSpeed(돌풍 속도) 
# WindDir(바람방향) WindSpeed(바람속도) Humidity(습도) Pressure(기압) 
# Cloud(구름) Temp(온도) RainToday(오늘 비 여부) RainTomorrow(내일 비 여부) 
#################################################################
# 날씨에 따라서 비가 내릴지의 여부를 기록한 데이터이다. 
# 이 데이터를 분석하면 어떤 날씨 조건에 비가 내릴지 또는 내리지 않을지에
# 대한 판단 기준을 분석할 수 있다.
#################################################################

# 1) 데이터 가져오기
# c:/Rwork/Part-IV/weather.csv 파일 선택
weather = read.csv("c:/NCS/Rwork/Part-IV/weather.csv", header=TRUE) 

# 2) 데이터 특성 보기
str(weather) # data.frame':  366 obs. of  15 variables:
names(weather) # 15개 변수명
head(weather)

# 3) 분류분석 - 의사결정트리 생성
weather.df <- rpart(RainTomorrow ~ ., 
                    data=weather[, c(-1,-14)], cp=0.01)
weather.df
# cp 속성 값을 높이면 가지 수가 적어지고, 낮추면 가지 수가 많아진다.
# cp 기본값은 0.01

# 4) 분류분석 시각화
X11()
plot(weather.df) # 트리 프레임 보임
text(weather.df, use.n=T, cex=0.5) # 텍스트 추가
post(weather.df, file="") # 타원제공 - rpart 패키지 제공 


# 5) 예측치 생성 
weather_pred <- predict(weather.df, weather)
weather_pred

# y의 범주로 코딩 변환 : Yes(0.5이상), No(0.5 미만)
weather_pred2 <- ifelse(weather_pred[,2] >= 0.5, 'Yes', 'No' )

class(weather_pred2)

# 6) 모델 평가(분류 정확도) 
res<-table(weather_pred2, weather$RainTomorrow)
(res[1,1]+res[2,2])/length(weather_pred2) # 0.9043716



