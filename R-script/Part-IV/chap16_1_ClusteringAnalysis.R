# chap16_1_ClusteringAnalysis

###################################################
# 군집분석(Clustering)
###################################################
# 고객DB   ->  알고리즘 -> 군집
# 알고리즘을 통해서(패턴으로) 근거리 모형으로 군집형성 - 규칙(rule)
# 변수에 의해서 그룹핑되다.
# 변수 적용 : 상품카테고리, 구매금액, 총거래금액

# 유사성 거리에 의한 유사객체를 묶어준다.
# 거리를 측정하여 집단의 이질성과 동질성을 평가하고, 이를 통해서 
# 군집을 형성한다..
# 유사성 거리 : 유클리드 거리
# y변수가 없는 데이터 마이닝 기법
# 예) 몸, 키 관점에서 묶음 -> 3개 군집 <- 3개 군집의 특징 요약
# 주요 알고리즘 : hierarchical, k-means

# 그룹화를 통한 예측(그룹 특성 차이 분석-고객집단 이해)

# 1. 유클리드 거리
# 유클리드 거리(Euclidean distance)는 두 점 사이의 거리를 계산하는 
# 방법으로 이 거리를 이용하여 유클리드 공간을 정의한다.

# (1) matrix 생성
x <- matrix(1:9, nrow=3, by=T) 
x
#      [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6
#[3,]    7    8    9


# (2) matrix 대상 유클리드 거리 생성 함수
# 형식) dist(x, method="euclidean") -> x : numeric matrix, data frame
dist <- dist(x, method="euclidean") # method 생략가능

dist
#          1         2
#2  5.196152          
#3 10.392305  5.196152


# (3) 유클리드 거리 계산 식

# 관측대상 p와 q의 대응하는 변량값의 차의 제곱의 합에 sqrt 적용

sqrt(sum((x[1,]-x[2,])^2))
sqrt(sum((x[1,]-x[3,])^2))


# <유클리드거리 계산법>
# 1. 관측대상의 두 벡터의 차이를 구한다.
# 2. 각 차의 제곱의 합을 구한다.
# 3. 제곱근을 취한다.
#------------------------------------------------------


# 2. 계층적 군집분석(탐색적 분석)
# - 계층적 군집분석(Hierarchical Clustering)
# - 거리가 가장 가까운 대상부터 결합하여 나무모양의 
#   계층구조를 상향식(Bottom-up)으로 만들어가면서 군집을 형성 

# (1) 군집분석(Clustering)분석을 위한 패키지 설치
install.packages("cluster") # hclust() : 계층적 클러스터 함수 제공
library(cluster) # 일반적으로 3~10개 그룹핑이 적정

# (2) 데이터 셋 생성
# x <- matrix(1:16, nrow=4) 
x <- matrix(1:9, nrow=3, by=T) 
x

# (3) matrix 대상 유클리드 거리 생성 함수
dist <- dist(x, method="euclidean") # method 생략가능
dist

# (4) 유클리드 거리 matrix를 이용한 클러스터링
hc <- hclust(dist) # 클러스터링 적용
hc
help(hclust)
plot(hc) # 클러스터 플로팅(Dendrogram) -> 1과2 군집(클러스터) 형성

#<실습> 중1학년 신체검사 결과 군집분석
#-------------------------------------
body <- read.csv("c:/NCS/Rwork/Part-Iv/bodycheck.csv", header=TRUE)
names(body)
body[,-1]
body
idist<- dist(body[, -1]) # dist(iris[, -5])
head(idist)
idist

hc <- hclust(idist)
hc

plot(hc,hang=-1) # 음수값 제외


# 3개 그룹 선정, 선 색 지정
rect.hclust(hc, k=3,border="red") # 3개 그룹 선정, 선 색 지정

# 각 그룹별 서브셋 만들기
g1<- subset(body, 번호==10| 번호==4| 번호==8| 번호==1 | 번호==15)
g2<- subset(body, 번호==11| 번호==3| 번호==5| 번호==6 | 번호==14)
g3<- subset(body, 번호==2| 번호==9| 번호==13| 번호==7 | 번호==12)

summary(g1);summary(g2);summary(g3)

hc$height
range(hc$height)

library(lattice)
count <- equal.count(hc$height,number=3,overlap=0)
count
class <-numeric()
rm(i)
for(i in 1:length(hc$height)){
  if(hc$height[i]>=11){
    class[i] <-3
  }else if(hc$height[i]>=5.4){
    class[i] <-2
  }else{
    class[i]<-1
  }
  
}
class



# 3. 계층형 군집분석에 그룹수 지정
# <실습> iris의 계층형군집결과에 그룹수를 지정하여 그룹수 만큼 
# 잘라서 iris의 1번째(Sepal.Length)와 3번째(Petal.Length) 변수를 
# 대상으로 클러스터별 변수의 평균 구하기 

# 1) 유클리드 거리 계산 
idist<- dist(iris[1:4]) # dist(iris[, -5])

# 2) 계층형 군집분석(클러스터링)
hc <- hclust(idist)
hc
plot(hc, hang=-1)
rect.hclust(hc, k=4, border="red") # 4개 그룹수 

# 3) 그룹수 만들기 : cutree()함수 -> 지정된 그룹수 만큼 자르기
# 형식) cutree(계층형군집결과, k=그룹수) -> 그룹수 만큼 자름
ghc<- cutree(hc, k=3) # stats 패키지 제공

ghc #  150개(그룹을 의미하는 숫자(1~3) 출력)

# 4) iris에서 ghc 컬럼 추가
iris$ghc <- ghc
table(iris$ghc) # ghc 빈도수
head(iris,60) # ghc 칼럼 확인 

# 5) 그룹별 요약통계량 구하기
g1 <- subset(iris, ghc==1)
summary(g1[1:4])

g2 <- subset(iris, ghc==2)
summary(g2[1:4])

g3 <- subset(iris, ghc==3)
summary(g3[1:4])


# [실습] 그룹별로 통계량 구하기 
# 5) 패키지 설치
install.packages("plyr")
library(plyr)

# 6) ddply() 함수 이용 - 그룹별 꽃받침 길이의 합, 꽃잎 길이의 표준편차  
# 형식) ddply(dataframe, .(집단변수), 요약집계, 컬럼명=함수(변수))
ddply(iris, .(ghc), summarize, Sepal.Length_sum=sum(Sepal.Length),
      Petal.Length_sd=sd(Petal.Length))

#  ghc Sepal.Length_sum Petal.Length_sd
#1   1            250.3       0.1736640
#2   2            471.3       0.6348989
#3   3            154.9       0.3900041
#------------------------------------------------------


# 4. 비계층적 군집분석(확인적 분석)
# - 군집 수를 알고 있는 경우 이용하는 군집분석 방법

# 군집분석 종류 : 계층적 군집분석(탐색적), 비계층적 군집분석(확인적) 

# 1) data set 준비 
library(ggplot2)
data(diamonds)

nrow(diamonds) # [1] 53940
t <- sample(1 : nrow(diamonds),1000) # 1000개 셈플링 

test <- diamonds[t, ] # 1000개 표본 추출
dim(test) # [1] 1000 10

head(test) # 검정 데이터
mydia <- test[c("price","carat", "depth", "table")] # 4개 칼럼만 선정
head(mydia)

# 2) 계층적 군집분석(탐색적 분석)
result <- hclust(dist(mydia), method="average") # 평균거리 이용 
result

# [작성] 군집 방법(Cluster method) 
# method = "complete" : 완전결합기준(최대거리 이용) <- default(생략 시)
# method = "single" : 단순결합기준(최소거리 이용) 
# method = "average" : 평균결합기준(평균거리 이용) 

plot(result, hang=-1) # hang : -1 이하 값 제거

# 3) 비계층적 군집분석(확인적 분석) - kmeans()함수 이용
# - 확인적 군집분석 : 군집의 수를 알고 있는 경우
result2 <- kmeans(mydia, 3)
result2 
# K-means clustering with 3 clusters of sizes 302, 95, 603 - 클러스터별 군집수 
# Cluster means: 클러스터별 칼럼의 평균 

names(result2) # cluster 칼럼 확인 
result2$cluster # 각 케이스에 대한 소속 군집수(1,2,3)

# 4) 원형데이터에 군집수 추가
mydia$cluster <- result2$cluster
head(mydia) # cluster 칼럼 확인 

# 5) 변수 간의 상관성 보기 
plot(mydia[,-5])
cor(mydia[,-5], method="pearson") # 상관계수 보기 
# 반응변수 : price <- 설명변수 : carat(양의 영향) > table(양의 영향) > depth(음의 영향)

library(corrgram) # 상관성 시각화 
corrgram(mydia[,-5]) # 색상 적용 - 동일 색상으로 그룹화 표시
corrgram(mydia[,-5], upper.panel=panel.conf) # 수치(상관계수) 추가(위쪽)


# 6) 비계층적 군집시각화
plot(mydia$carat, mydia$price)
plot(mydia$carat, mydia$price, col=mydia$cluster)
# mydia$cluster 변수로 색상 지정(1,2,3)

# 중심점 표시 추가
result2$centers # Cluster means 값을 갖는 컬럼 
#     price    carat    depth    table
#1  5491.487 1.099205 61.86391 57.77715
#2 13103.337 1.728000 61.55789 57.78211
#3  1446.033 0.487728 61.82222 56.99701

# 각 그룹의 중심점에 포인트 추가 
points(result2$centers[,c("carat", "price")], col=c(3,1,2), pch=8, cex=5)
# names(result2) -> centers 칼럼 확인 
# col : color, pch : 중심점 문자, cex : 중심점 문자 크기
# pch(plotting character), cex(character expansion)







