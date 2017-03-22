# chap14_1_Factor Analysis

###########################################
# chap14_1. 요인분석(Factor Analysis)
###########################################

# 요인분석의 목적 
# 1. 자료의 요약 : 변인을 몇 개의 공통된 변인으로 묶음  
# 2. 변인 구조 파악 : 변인들의 상호관계 파악(독립성 등) 
# 3. 불필요한 변인 제거 : 중요도가 떨어진 변수 제거 
# 4. 측정도구 타당성 검증 : 변인들이 동일한 용인으로 묶이는지 여부를 확인 

# 전제조건 : 등간척도 or 비율척도, 정규분포, 관찰치 상호독립적/분산 동일  

# 요인분석 결과에 대한 활용 방안 
# 1. 서로 밀접하게 관련된 변수들을 합치거나 중복된 변수를 제거하여 변수를 축소한다.
# 2. 변수들 간의 연관성 또는 공통점 탐색 
# 3. 요인점수 계산으로 상관분석, 회귀분석의 설명변수로 이용 

###########################
### 요인분석 실습 
###########################
# 실습 목적 : 6개 과목 점수를 대상으로 요인분석하여 과목 수 줄임    
# 변수 설명 : 6개 과목의 점수(5점 만점 = 5점 척도) 
# s1 : 자연과학, s2 : 물리화학
# s3 : 인문사회, s4 : 신문방송
# s5 : 응용수학, s6 : 추론통계
s1 <- c(1, 2, 1, 2, 3, 4, 2, 3, 4, 5)
s2 <- c(1, 3, 1, 2, 3, 4, 2, 4, 3, 4)
s3 <- c(2, 3, 2, 3, 2, 3, 5, 3, 4, 2)
s4 <- c(2, 4, 2, 3, 2, 3, 5, 3, 4, 1)
s5 <- c(4, 5, 4, 5, 2, 1, 5, 2, 4, 3)
s6 <- c(4, 3, 4, 4, 2, 1, 5, 2, 4, 2)
name <-1:10 


subject <- data.frame(s1, s2, s3, s4, s5, s6)
subject
str(subject) 
summary(subject)


# 1. 주성분분석 : 차원수를 줄일 때 사용
# - 요인의 개수를 정할 때 사용 
# - 데이터셋을 구성하는 변수 중에서 불필요한 변수 확인
# - 변동량에 영향을 주는 성분의 개수를 알아볼 때 사용 

pc <- prcomp(subject) # scale = TRUE
summary(pc)

plot(pc)
biplot(pc) #주성분 분석 시각화 

# 상관관계 행렬 대상 고유값 계산  
cor(subject) # 6개 과목변수 간의 상관계수 보기 

# eigen()함수를 이용하여 상관계수 행렬을 대상으로 고유값 계산 
en <- eigen(cor(subject)) # $values : 고유값, $vectors : 고유벡터  

#$values : 고유값(스칼라) 보기
en$values # $values : 고유값(스칼라) 보기  
# - 고유값이란 어떤 행렬(상관관계수 행렬)로부터 유도되는 특정한 실수값

plot(en$values, type="o") # 고유값을 이용한 시각화 

# 2. 상관관계 분석 : 요인분석은 변수간의 상관성으로 공통점 인식  
cor(subject) 

# 3. 요인분석 : 요인회전법 적용(varimax is the default) 
# 요인회전법 : 요인 해석이 어려운 경우 어느 한 요인에 높게 나타나도록 하기 위해 요인축 회전

# (1) 주성분분석의 가정에 의해서 2개 요인으로 분석  p-value is 0.0232 <0.05 factor2개가 적절하지 않다.
result <- factanal(subject, factors = 2, rotation = "varimax")
result

# (2) 요인수를 3개로 지정  the fit was 0.7745 
?factanal
result <- factanal(subject, factors = 3, # 요인 개수 지정 
                   rotation = "varimax", # 회전방법 지정("varimax", "promax", "none")
                   scores="regression") # 요인점수 계산 방법
result

# 요인적재량 보기  
attributes(result)
result$loadings
result$scores

# 요인부하량 0.5 이상, 소수점 2자리 표기 
print(result, digits = 2, cutoff=0.5) 

# 모든 요인적재량 보기 :감추어진 요인적재량 보기
print(result$loadings, cutoff=0) # display every loadings


# 4. 요인적재량으로 요인점수 시각화 

# 1) Factor1, Factor2 요인지표 시각화 
# 요인점수행렬(= 관측치 수) 
plot(result$scores[,c(1:2)], main="Factor1과 Factor2 요인점수 행렬")
# row = Factor1, column = Factor2

# 관측치별 이름 매핑(rownames mapping)
text(result$scores[,1], result$scores[,2], 
     labels = name, cex = 0.7, pos = 3, col = "blue")

# 요인적재량 plotting
points(result$loadings[,c(1:2)], pch=19, col = "red")

text(result$loadings[,1], result$loadings[,2], 
     labels = rownames(result$loadings), 
     cex = 0.8, pos = 3, col = "red")

# 2) Factor1, Factor3 요인지표 시각화 
# 요인점수행렬(= 관측치 수) 
plot(result$scores[,c(1,3)], main="Factor1과 Factor3 요인점수 행렬")

# 관측치별 이름 매핑(rownames mapping)
text(result$scores[,1], result$scores[,3], 
     labels = name, cex = 0.7, pos = 3, col = "blue")

# 요인적재량 plotting
points(result$loadings[,c(1,3)], pch=19, col = "red")
# Factor1, Factor3 요인적재량 표시  

text(result$loadings[,1], result$loadings[,3], 
     labels = rownames(result$loadings), 
     cex = 0.8, pos = 3, col = "red")

#######################
## 3차원 시각화 
#######################
library(scatterplot3d)

Factor1 <- result$scores[,1]
Factor2 <- result$scores[,2]
Factor3 <- result$scores[,3] 
# scatterplot3d(밑변, 오른쪽변, 왼쪽변, type='p') # type='p' : 기본산점도 표시 
d3 <- scatterplot3d(Factor1, Factor2, Factor3)

# 요인적재량 표시 
loadings1 <- result$loadings[,1]
loadings2 <- result$loadings[,2]
loadings3 <- result$loadings[,3] 
d3$points3d(loadings1, loadings2, loadings3, bg='red',pch=21, cex=2, type='h')

# 6. 요인축소 : 요인분석 지표 이용 요인 축소  
# 계산식 =  sum(변수i의 요인적재량 * 변수i 값)
# 계산식 통해서 3개의 요인으로 차원 축소

Factor1 <- -0.379*1 + -0.710*1  + 0.236*2 + 0.120*2 + 0.7712*4  + 0.900*4 
Factor1 #  6.3078
Factor2 <- -0.005*1 + 0.140*1  + 0.931*2 + 0.983*2 + 0.297*4  + 0.301*4 
Factor2 # 6.355
Factor3 <- 0.923*1 + 0.649*1  + 0.166*2 + -0.118*2 + -0.278*4  + -0.307*4 
Factor3 # -0.672

loadings <- result$loadings # 요인부하량 
dim(loadings) # 6 3

dim(subject) #10 6

app_science <- numeric() # 응용과학 
soc_science <- numeric() # 사회과학 
net_science <- numeric() # 자연과학 

for(i in 1:nrow(subject)){ # 10회 반복 
  fs1_sum <- 0; fs2_sum <- 0; fs3_sum <- 0
  
  for(j in 1:ncol(subject)){ # 6회 반복 
    fs1_sum <- fs1_sum + (loadings[j,1] * subject[i, j]) # Factor1 : '응용과학' 지표 
    fs2_sum <- fs2_sum + (loadings[j,2] * subject[i, j]) # Factor2 : '사회과학' 지표
    fs3_sum <- fs3_sum + (loadings[j,3] * subject[i, j]) # Factor3 : '자연과학' 지표
  }
  # 개인별 요인점수
  app_science[i] = fs1_sum
  soc_science[i] = fs2_sum
  net_science[i] = fs3_sum
}

app_science
soc_science
net_science

fecter_df <- data.frame(app_science,soc_science,net_science)
cor(fecter_df)


###########################
### 요인분석 실습2 
###########################
# 잘못 분류된 요인 제거로 변수 정제

# (1) 데이터 가져오기 
install.packages("memisc")
library(memisc)
setwd("C:\\NCS\\Rwork\\Part-III")
data.spss <- as.data.set(spss.system.file('drinking_water.sav'))
data.spss
drinking_water <- data.spss[1:11]
drinking_water
drinking_water_df <- as.data.frame(drinking_water) 
str(drinking_water_df)
# 친밀도 : q1,q2,q3,q4
# 적절성 : q5,q6,q7
# 만족도 : q8,q9,q10,q11
drinking_water_df[1:4]

# (2) 초기고유값으로 요인 수  
en2 <- eigen(cor(drinking_water_df))
plot(en2$values, type="o") # 요인수(3)를 알고 있는 경우 

# (3) 3개 요인으로 요인분석 
result2 <- factanal(drinking_water_df, factors = 3, rotation = "varimax",
                    scores = "regression")
result2 # p-value is 0.0255  <0.05 요인 3개는 적절하지 않다.

print(result2, cutoff=0.5)

result2$scores

# 4. 요인축소 

# 요인적재량 행렬 칼럼명 변경 
factor_load <- result2$loadings
colnames(factor_load) <- c("제품만족도","제품친밀도","제품적절성")
factor_data <- factor_load[1:11,]  
factor_data

# 요인적재량에서 요인별로 추출(q4변수 제거)
factor1 <- factor_data[c(1:3,5:11), 1] # 만족도(q4 제거) 
factor2 <- factor_data[c(1:3,5:11), 2] # 친밀도(q4 제거) 
factor3 <- factor_data[c(1:3,5:11), 3] # 적절성(q4 제거) 

factor1; factor2; factor3

dim(drinking_water_df) # [1] 380  11
dw_df <-drinking_water_df[,c(1:3,5:11)]
str(dw_df) # 'data.frame':	380 obs. of  10 variables:

satisfaction <- numeric() # 만족도 점수가 저장될 벡터 변수
closeness <- numeric() # 친밀도 점수가 저장될 벡터 변수
pertinence <- numeric() # 친밀도 점수가 저장될 벡터 변수 

for( i in 1:nrow(dw_df) ) { # 380
  fs1 <- 0; fs2 <- 0; fs3 <- 0
  for (j in 1:ncol(dw_df)) { # 10
    fs1 <- fs1 + factor1[j] * dw_df[i, j] 
    fs2 <- fs2 + factor2[j] * dw_df[i, j]
    fs3 <- fs3 + factor3[j] * dw_df[i, j]
  }
  satisfaction[i] = fs1; closeness[i] = fs2; pertinence[i] = fs3
}

satisfaction[1:5] # 14.474989 11.731702 16.344102  9.132778  9.142907

closeness[1:5] # 11.675810 11.633331 13.438303 10.065605  9.884828

pertinence[1:5] # 11.749477 10.068419 13.027469  8.280897  8.487258

drinking_water_result <- data.frame(satisfaction,closeness,pertinence)
head(drinking_water_result)

cor(drinking_water_result)
