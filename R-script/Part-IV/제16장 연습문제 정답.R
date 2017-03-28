#################################
## <제16장 연습문제>
################################# 

# 01. iris 데이터 셋의 1~4번째 변수를 대상으로 유클리드 거리 매트릭스를 구하여 
# idist에 저장한 후 계층적 클러스터링을 적용하여 결과를 시각화 하시오.

iris[1:4] # 4개변수 전체


#단계1. 유클리드 거리 계산
idist<- dist(iris[1:4]) # or dist(iris[, -5])
head(idist)

#단계2. 계층형 군집분석(클러스터링)
hc <- hclust(idist)
hc
#Cluster method   : complete 
#Distance         : euclidean 
#Number of objects: 150 

#단계3. 분류결과를 대상으로 음수값을 제거하여 덴드로그램 시각화
plot(hc, hang=-1) # 계층적 clustering 그래프 : 덴드로그램
# hang=-1 : 음수값 제거

#단계4. 그룹수를 4개로 지정하고 그룹별로 테두리 표시
rect.hclust(hc, k=4, border="red") # 4개 그룹 선정, 선 색 지정


# 02. 다음과 같은 조건을 이용하여 각 단계별로 비계층적 군집분석을 수행하시오.

# 조건1) 대상 파일 : c:/Rwork/Part-IV/product_sales.csv
# 조건2) 변수 설명 : tot_price : 총구매액, buy_count : 구매횟수, 
#                    visit_count : 매장방문횟수, avg_price : 평균구매액

sales <- read.csv("c:/Rwork/Part-IV/product_sales.csv", header=TRUE)
head(sales) 


# 단계1: 비계층적 군집분석 : 3개 군집으로 군집화
model <- kmeans(sales,3) # kmeans(data, k) : k개수: 군집수
model # 원형데이터를 대상으로 3개 군집으로 군집화

# 각 케이스에 대한 소속 군집수(1,2,3) 확인
model$cluster # 각 케이스에 대한 소속 군집수(1,2,3)

# 단계2: 원형데이터에 군집수 추가
sales$group <- model$cluster
head(sales)# group 추가

# 단계3 : tot_price 변수와 가장 상관계수가 높은 변수와 군집분석 시각화
# (1) 상관관계 분석 
cor(sales[,-5], method="pearson")
# <해설> tot_price에 가장 큰 영향을 미치는 변수는 avg_price

# (2) 비계층적 군집분석 시각화 : 그룹으로 색상 표시 
plot(sales[c("tot_price", "avg_price")], col=sales$group)

# 단계4. 군집의 중심점 표시
points(result2$centers[,c("tot_price", "avg_price")], col=1:3, pch=8, cex=2)


# 03. tranExam.csv 파일을 대상으로 중복된 트랜잭션 없이  
# 1~2컬럼만 single 형식으로 트랜잭션 객체를 생성하시오.
# (파일경로 : C:/Rwork/Part-IV/tranExam.csv)

# 단계1 : 트랜잭션 객체 생성 및 확인
# 단계2 : 각 item별로 빈도수 확인
# 단계3 : 파라미터(supp=0.3, conf=0.1)를 이용하여 규칙(rule) 생성 
# 단계4 : 연관규칙 결과 보기

# 단계1 : 트랜잭션 객체 생성 및 확인
library(arules)
tranExam <- read.transactions("C:/Rwork/Part-IV/tranExam.csv", format="single", 
                              sep=",", cols=c(1,2), rm.duplicates=T)
tranExam
# 트랜잭션 데이터 보기
inspect(tranExam) 

# 단계2 : 각 item별로 빈도수 확인
summary(tranExam) 

# 단계3 : 파라미터(supp=0.3, conf=0.1)를 이용하여 규칙(rule) 생성 
ruleExam<- apriori(tranExam) #set of 10 rules
ruleExam <- apriori(tranExam, parameter = list(supp=0.3, conf=0.1)) # 12 rule

# 단계4 : 연관규칙 결과 보기
inspect(ruleExam) # 12개 연관규칙


# 04. Adult 데이터셋을 대상으로 다음 단계별로 연관분석을 수행하시오.

# 단계1: 최소 support=0.5, 최소 confidence=0.9를 지정하여 연관규칙 생성
data(Adult)
library(arulesViz)

adult <- apriori(Adult, parameter = list(supp=0.5, conf=0.9))
adult # set of 52 rules 

# 단계2: 수행한 결과를 lift 기준으로 정렬하여 상위 10개 규칙 확인
rules<- inspect(head(sort(adult, by="lift"),10))

# 단계3: 연관분석 결과를  LHS와 RHS의 빈도수로 시각화 
plot(adult) # 지지도, 신뢰도 향상도 산점도  
plot(adult, method="grouped") # LHS와 RHS 간의 빈도수 시각화


# 단계4: 연관분석 결과를 연관어 네트워크 형태로 시각화
plot(adult, method="graph", control=list(type="items")) 

# 단계5: 연관어 중심 단어 해설
# <해설> 자본이익과 자본손실의 단어를 중심으로 연관어가 형성되어 있다.


# 05. Adult 데이터셋을 대상으로 다음 단계별로 연관분석을 수행하시오.

# 단계1 : support=0.3, confidence=0.95가 되도록 연관규칙 생성
AR <- apriori(Adult, parameter = list(supp=0.3, conf=0.95))
AR # set of 124 rules 
inspect(AR) # 연관규칙 확인 

#  단계2 : 왼쪽 item이 백인(White)인 규칙만 서브셋으로 작성하고, 시각화
race <- subset(AR, lhs %in% 'race=White')
race # set of 46 rules 
inspect(race)
plot(race, method="graph") #  연관어 네트워크 시각화 

#  단계3 : 왼쪽 item이 백인이거나 미국인을 대상으로 서브셋을 작성하고, 시각화
race_country <- subset(AR, lhs %in% c('native-country=United-States','race=White'))
race_country # set of 76 rules 
inspect(race_country)
plot(race_country, method="graph")

#  단계4 : 오른쪽 item에서 'Husband' 단어를 포함 규칙을 서브셋으로 작성하고, 시각화
husband <- subset(AR, rhs %pin% 'Husband')
husband # set of 12 rules 
plot(husband, method="graph")
