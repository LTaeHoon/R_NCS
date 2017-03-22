#chap16_2_AssociationAnalysis

###################################################
# 연관분석(Association Analysis)
###################################################
# 군집분석 : 그룹핑 하는 작업
#     ↓
# 연관분석 : 그룹에 대한 특성 분석(장바구니 분석)
#--------------------------------------------------

# 연관분석은 군집분석에 의해서 그룹핑된 cluster를 대상으로 해당
# 그룹에 대한 특성을 분석하는 방법으로 장바구니 분석으로 알려짐
# 즉 유사한 개체들을 클러터로 그룹화하여 각 집단의 특성 파악
# 대용량 데이터베이스에서는 전체 데이터를 유사한 클러스터로 
# 묶어서 관찰 및 분석하는 것이 더 효율적이다.

# 특징
# - 데이터베이스에서 사건의 연관규칙을 찾는 무방향성 데이터마이닝 기법                                            
# - 무방향성(x -> y변수 없음) -> 비지도 학습에 의한 패턴 분석 방법
# - 사건과 사건 간 연관성(관계)를 찾는 방법(예:기저귀와 맥주)
# - A와 B 제품 동시 구매 패턴(지지도)
# - A 제품 구매 시 B 제품 구매 패턴(신뢰도)


# 예) 장바구니 분석 : 장바구니 정보를 트랜잭션(상품 거래 정보)이라고 하며,
# 트랜잭션 내의 연관성을 살펴보는 분석기법
# 분석절차 : 거래내역 -> 품목 관찰 -> 상품 연관성에 대한 규칙(Rule) 발견

# 활용분야
# - 대형 마트, 백화점, 쇼핑몰 등에서 고객의 장바구니에 들어있는 품목 간의 
#   관계를 탐구하는 용도
# ex) 고객들은 어떤 상품들을 동시에 구매하는가?
#   - 맥주를 구매한 고객은 주로 어떤 상품을 함께 구매하는가?

# - 대형 마트,백화점, 쇼핑몰 판매자 -> 고객 대상 상품추천, 마케팅
# 1) 고객 대상 상품추천 및 상품정보 발송  
#     -> ex) A고객에 대한 B 상품 쿠폰 발송
# 2) 텔레마케팅를 통해서 패키지 상품 판매 기획 및 홍보 
# 3) 상품 진열 및 show window 상품 display


# 연관규칙(Association Rule)
# - 상업 데이터베이스에서 가장 흔히 쓰이는 도구로, 
# - 어떤 사건이 얼마나 자주 동시에 발생하는가를 표현하는 규칙(조건)

#########################################
# 1. 연관규칙 평가 척도
#########################################

# 연관규칙의 평가 척도
# 1. 지지도(support) : 전체자료에서 A를 구매한 후 B를 구매하는 거래 비율 
#  A->B 지지도 식 
#  -> A와 B를 포함한 거래수 / 전체 거래수
#  -> n(A, B) : 두 항목(A,B)이 동시에 포함되는 거래수
#  -> n : 전체 거래 수

# 2. 신뢰도(confidence) : A가 포함된 거래 중에서 B를 포함한 거래의 비율(조건부 확률)
# A->B 신뢰도 식
#  -> A와 B를 포함한 거래수 / A를 포함한 거래수

# 3. 향상도(Lift) : 하위 항목들이 독립에서 얼마나 벗어나는지의 정도를 측정한 값
# 향상도 식
#  -> 신뢰도 / B가 포함될 거래율
# 분자와 분모가 동일한 경우 : Lift == 1, A와 B가 독립(상관없음)
# 분자와 분모가 동일한 경우 : Lift != 1, x와 y가 독립이 아닌 경우(상관있음)

# 향상도가 1에 가까우면 : 두 상품이 독립(과자와 후추)
# 1보다 작으면 : 두 상품이 음의 상관성(설사약과 변비약)
# 1보다 크면 : 두 상품이 양의 상관성(빵과 버터)
# 연관규칙에 의미가 있으려면 1보다 큰 값이어야 한다.
# 값이 높을 수록 상품 간의 연관성이 높다.
# 1 이상이 양의 상관관계, 0이하는 음의 상관관계 


# <지지도와 신뢰도 예시>
# t1 : 라면,맥주,우유
# t2 : 라면,고기,우유
# t3 : 라면,과일,고기
# t4 : 고기,맥주,우유
# t5 : 라면,고기,우유
# t6 : 과일,우유

# 중복되지 않는 item 개수는 5개

#         A -> B          지지도       신뢰도        향상도
#  맥주 -> 고기         1/6=0.166      1/2=0.5      0.5/0.66(4/6)=0.75
#  라면,우유 -> 맥주    1/6=0.166      1/3= 0.33   0.33/0.33(2/6) =1

## 연관성 규칙 분석을 위한 패키지
install.packages("arules") # association Rule
# read.transactions(),  apriori(), Adult 데이터셋 제공
library(arules) #read.transactions()함수 제공


# 1. transaction 객체 생성(파일 이용)
setwd("c:/NCS/Rwork/Part-IV")
tran<- read.transactions("tran.txt", format="basket", sep=",")
tran

# 2. transaction 데이터 보기
inspect(tran)

# 3. rule 발견(생성) - 지지도,신뢰도 = 0.1
# apriori(트랜잭션 data, parameter=list(supp, conf))

# 연관성 규칙 평가 척도 - 지지도와 신뢰도
# apriori 함수는 transaction 데이터만 받을 수 있음
rule<- apriori(tran, parameter = list(supp=0.3, conf=0.1)) # 16 rule
rule<- apriori(tran, parameter = list(supp=0.1, conf=0.1)) # 35 rule 
inspect(rule) # 규칙 보기

# apriori()함수 도움말 
help("apriori") # support 0.1, confidence 0.8, and maxlen 10 
rule <- apriori(tran) # [6 rule(s)]
rule<- apriori(tran, parameter = list(supp=0.1, conf=0.8, maxlen=10)) # 6 rule 
inspect(rule) 
# <해설> maxlen 속성 : lhs와 rhs를 합쳐서 10이하의 길이 갖는 조건  


#########################################
# 2. 트랜잭션 객체 생성 
#########################################

#형식)
#read.transactions(file, format=c("basket", "single"),
#      sep = NULL, cols=NULL, rm.duplicates=FALSE,encoding="unknown")
#------------------------------------------------------
#file : file name
#format : data set의 형식 지정(basket 또는 single)
# -> single : 데이터 구성(2개 칼럼) -> transaction ID에 의해서 상품(item)이 대응된 경우
# -> basket : 데이터 셋이 여러개의 상품으로 구성 -> transaction ID 없이 여러 상품(item) 구성
#sep : 상품 구분자
#cols : single인 경우 읽을 컬럼 수 지정, basket은 생략(transaction ID가 없는 경우)
#rm.duplicates : 중복 트랜잭션 항목 제거
#encoding : 인코딩 지정
#------------------------------------------------------

# (1) single 트랜잭션 객체 생성
## read demo data - sep 생략 : 공백으로 처리, single인 경우 cols 지정 
# format = "single" : 1개의 transaction id에 의해서 item이 연결된 경우 
setwd("c:/NCS/Rwork/Part-IV")
stran <- read.transactions("demo_single",format="single",cols=c(1,2)) 
inspect(stran)

# <실습> 중복 트랜잭션 객체 삭제 
setwd("c:/NCS/Rwork/Part-IV")
stran2<- read.transactions("single_format.csv", format="single", sep=",", 
                           cols=c(1,2), rm.duplicates=T)
stran2
inspect(stran2)
summary(stran2) # 248개 트랜잭션에 대한 기술통계 제공
#sizes
#1   2   3   4   5   6   7   8  <- item 개수
#12  25  16  20 119  12  37   7 <- 248
# item수가 1개로 구성된 트랜잭션이 12개 라는 의미


# 트랜잭션 보기
inspect(stran2) # 248 트랜잭션 확인 

# 규칙 발견
astran2 <- apriori(stran2) # supp=0.1, conf=0.8와 동일함 
#astran2 <- apriori(stran2, parameter = list(supp=0.1, conf=0.8))
astran2 # set of 102 rules
attributes(astran2)
inspect(astran2)

# 향상도가 높은 순서로 정렬 
inspect(sort(astran2, by="lift"))
inspect(head(sort(astran2, by="lift")))

# (2) basket 트랜잭션 데이터 가져오기
btran <- read.transactions("c:/NCS/Rwork/Part-IV/demo_basket",format="basket",sep=",") 
inspect(btran) # 트랜잭션 데이터 보기


##############################################
# 3. 연관규칙 시각화(Adult 데이터 셋 이용)
##############################################

data(Adult) # arules에서 제공되는 내장 데이터 로딩
str(Adult) # Formal class 'transactions' , 48842(행)
# 'transactions' : 고객 물품내역으로 작성
Adult
#transactions in sparse format with
#48842 transactions (rows) and
#115 items (columns)
attributes(Adult)# 트랜잭션의 변수와 범주 보기
################ Adult 데이터 셋 #################
# 32,000개의 관찰치와 15개의 변수로 구성되어 있음
# 종속변수에 의해서 년간 개인 수입이 5만달러이상 인지를
# 예측하는 데이터 셋으로 transactions 데이터로 읽어온
# 경우 48,842행과 115 항목으로 구성된다.
##################################################


# [data.frame 형식으로 보기] - 트랜잭션 데이터를 데이터프레임으로 변경 
adult<- as(Adult, "data.frame") # data.frame형식으로 변경 
str(adult) # 'data.frame':	48842 obs. of  2 variables:
head(adult) # 칼럼 내용 보기 

# 요약 통계량
summary(Adult)
#sizes
#9    10    11    12    13  <- itemset()
#19   971  2067 15623 30162 <- transaction(48842)


----------------------------------------------------
  # 신뢰도 80%, 지지도 10%이 적용된 연관규칙 6137 발견   
  ----------------------------------------------------
  
  ar<- apriori(Adult, parameter = list(supp=0.1, conf=0.8))

ar1<- apriori(Adult, parameter = list(supp=0.2)) # 지도도 높임

ar2<- apriori(Adult, parameter = list(supp=0.2, conf=0.95)) # 신뢰도 높임

ar3<- apriori(Adult, parameter = list(supp=0.3, conf=0.95)) # 신뢰도 높임

ar4<- apriori(Adult, parameter = list(supp=0.35, conf=0.95)) # 신뢰도 높임

ar5<- apriori(Adult, parameter = list(supp=0.4, conf=0.95)) # 신뢰도 높임

# 결과보기
inspect(head(ar5)) # 상위 6개 규칙 제공 -> inspect() 적용

# confidence(신뢰도) 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(ar5, decreasing=T, by="confidence")))

# lift(향상도) 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(ar5, by="lift"))) 


## 연관성 규칙에 대한 데이터 시각화를 위한 패키지
install.packages("arulesViz") 
library(arulesViz) # rules값 대상 그래프를 그리는 패키지

plot(ar3) # 지지도(support), 신뢰도(conf) , 향상도(lift)에 대한 산포도
plot(ar4,method="grouped") # lhs -> rhs
plot(ar5, method="graph") #  연관규칙 네트워크 그래프
# 각 연관규칙 별로 연관성 있는 항목(item) 끼리 묶여서 네트워크 형태로 시각화
