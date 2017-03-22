#################################
## <제16장 연습문제>
################################# 

# 01. iris 데이터 셋의 1~4번째 변수를 대상으로 유클리드 거리 매트릭스를 구하여 
# idist에 저장한 후 계층적 클러스터링을 적용하여 결과를 시각화 하시오.

data<-iris[1:4] # 4개변수 전체

summary(iris[1:4])
#단계1. 유클리드 거리 계산
idist <- dist(iris[1:4],method="euclidean")

#단계2. 계층형 군집분석(클러스터링)
hc_iris <- hclust(idist)
hc_iris
attributes(hc_iris)
#단계3. 분류결과를 대상으로 음수값을 제거하여 덴드로그램 시각화
plot(hc_iris,hang=-1)
#단계4. 그룹수를 4개로 지정하고 그룹별로 테두리 표시
rect.hclust(hc_iris,h = 3,border = 'red')

cut<-cutree(hc_iris, h = 3)
g1 <- data[which(cut==1),]
g2 <- data[which(cut==2),]
g3 <- data[which(cut==3),]
g4 <- data[which(cut==4),]
summary(g1);summary(g2);summary(g3);summary(g4)

# 02. 다음과 같은 조건을 이용하여 각 단계별로 비계층적 군집분석을 수행하시오.

# 조건1) 대상 파일 : c:/Rwork/Part-IV/product_sales.csv
# 조건2) 변수 설명 : tot_price : 총구매액, buy_count : 구매횟수, 
#                    visit_count : 매장방문횟수, avg_price : 평균구매액

sales <- read.csv("c:/NCS/Rwork/Part-IV/product_sales.csv", header=TRUE)
head(sales) 

# 단계1: 비계층적 군집분석 : 3개 군집으로 군집화
ksales<-kmeans(sales,3)
ksales$cluster
# 단계2: 원형데이터에 군집수 추가
sales$class <- ksales$cluster
table(sales$class)
cor(sales[,1:4])

# 단계3 : tot_price 변수와 가장 상관계수가 높은 변수와 군집분석 시각화
plot(sales$avg_price,sales$tot_price,col=ksales$cluster)
# 단계4. 군집의 중심점 표시
points(ksales$centers[,c("avg_price","tot_price")],pch=5,cex=2,col=c(1,2,3))

# 03. tranExam.csv 파일을 대상으로 중복된 트랜잭션 없이  
# 1~2컬럼만 single 형식으로 트랜잭션 객체를 생성하시오.
# (파일경로 : C:/Rwork/Part-IV/tranExam.csv)
library(arules)
setwd("c:/NCS/Rwork/Part-IV")
stran_exam<- read.transactions("tranExam.csv", format="single", sep=",", 
                           cols=c(1,2), rm.duplicates=T)

# 단계1 : 트랜잭션 객체 생성 및 확인
inspect(stran_exam)
# 단계2 : 각 item별로 빈도수 확인
summary(stran_exam)
# sizes
# 2 4 
# 4 1 

# 단계3 : 파라미터(supp=0.3, conf=0.1)를 이용하여 규칙(rule) 생성 
stran_arules<-apriori(stran_exam,parameter=list(supp=0.3,conf=0.1))
# 단계4 : 연관규칙 결과 보기
stran_arules
inspect(stran_arules)
inspect(sort(stran_arules,by="lift"))

# 04. Adult 데이터셋을 대상으로 다음 단계별로 연관분석을 수행하시오.

# 단계1: 최소 support=0.5, 최소 confidence=0.9를 지정하여 연관규칙 생성

# 단계2: 수행한 결과를 lift 기준으로 정렬하여 상위 10개 규칙 확인

# 단계3: 연관분석 결과를  LHS와 RHS의 빈도수로 시각화 

# 단계4: 연관분석 결과를 연관어 네트워크 형태로 시각화

# 단계5: 연관어 중심 단어 해설

# 05. Adult 데이터셋을 대상으로 다음 단계별로 연관분석을 수행하시오.

# 단계1 : support=0.3, confidence=0.95가 되도록 연관규칙 생성

#  단계2 : 왼쪽 item이 백인(White)인 규칙만 서브셋으로 작성하고, 시각화

#  단계3 : 왼쪽 item이 백인이거나 미국인을 대상으로 서브셋을 작성하고, 시각화

#  단계4 : 오른쪽 item에서 'Husband' 단어를 포함 규칙을 서브셋으로 작성하고, 시각화
