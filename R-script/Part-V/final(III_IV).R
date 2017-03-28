################################
### Part-III, IV 총정리
################################

election3 <- read.csv(file.choose()) # 2012년도 미국 대통령 선거 후원금 현황 
str(election3) # 'data.frame':	1001731 obs. of  16 variables:
# 2.cand_id : 대선후보자
# 3.cand_nm : 대선 후보자 이름
# 4.contbr_nm  : 후원자 이름 
# 9.contbr_occupation : 후원자 직업군 
# 10.contb_receipt_amt : 후원금

# 위 7개 칼럼으로 data.frame 생성(election_df3) 
election_df3 <- election3[c(2:4,9:10)]  
str(election_df3) # 'data.frame':	1001731 obs. of  5 variables:
dim(election_df3) # 1001731       5
summary(election_df3) # contbr_occupation 칼럼 NA(128) 있음 
#---------------------------------------------------------------------

# data 전처리 : NA 제거, 50,000개 샘플링, 후보자별 서브셋 생성
clean_election <- subset(election_df3, !is.na(election_df3$contbr_occupation)) 
dim(clean_election) # # 1001603      5 -> NA(128) 제거됨 

idx <- sample(nrow(clean_election), 50000)
length(idx) # 50000
clean_election2 <- clean_election[idx, ]
nrow(clean_election2)

romney <- subset(clean_election2, cand_nm=='Romney, Mitt')
obama <- subset(clean_election2, cand_nm == 'Obama, Barack')
dim(romney) # 5378    5
dim(obama) # 29596     5

# data.frame 병합(merge)
obama_romney  <- rbind(obama, romney)
head(obama_romney)
tail(obama_romney)

## chapter 10 : 교차분석 ## 

# 문1) 후원자의 직업군과 대통령 당선 유무에 따라서 다음과 같이 교차테이블을 작성하시오.
# <조건1> 대상 변수 : obama_romney
# <조건2> 후원자의 직업군 다음과 같이 job1, job2, job3 코딩 변경 후 contbr_occupation2 칼럼 저장 
# job1 : INVESTOR - 투자자, EXECUTIVE - 경영진, PRESIDENT  - 회장 
# job2 : LAWYER - 변호사, PHYSICIAN  - 내과의사, ATTORNEY   - 변호사
# job3 : RETIRED  - 퇴직자, HOMEMAKER  - 주부

obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='INVESTOR'] <- 'job1'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='EXECUTIVE'] <- 'job1'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='PRESIDENT'] <- 'job1'

obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='LAWYER'] <- 'job2'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='PHYSICIAN'] <- 'job2'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='ATTORNEY'] <- 'job2'

obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='RETIRED'] <- 'job3'
obama_romney$contbr_occupation2[obama_romney$contbr_occupation=='HOMEMAKER'] <- 'job3'

# <조건3> contbr_occupation2 칼럼에 NA를 포함한 관측치 제거 후 서브셋 작성(obama_romney2 저장)  
table(obama_romney$contbr_occupation2, useNA = 'ifany') 
obama_romney2 <- subset(obama_romney, !is.na(obama_romney$contbr_occupation2))

# <조건4> obama_romney2 변수를 대상으로 cand_nm 칼럼이 'Obama, Barack'이면 '당선', 
#               'Romney, Mitt'이면 '낙선'으로 파생변수를 생성하여 cand_pass 칼럼 추가
obama_romney2$cand_pass[obama_romney2$cand_nm=='Obama, Barack'] <- '당선'
obama_romney2$cand_pass[obama_romney2$cand_nm=='Romney, Mitt'] <- '낙선'


# <조건5> 직업유형과 당선유무 칼럼으로 교차분할표 작성  
table(obama_romney2$contbr_occupation2, obama_romney2$cand_pass)


## chapter 10 : 카이스케어 검정 ##

# 문2) 후원자의 직업군과 대통령 당선 유무 여부와 관련성이 있는가를 검정하시오.
# <조건1> 대상 변수 : obama_romney2
# <조건2> 귀무가설과 대립가설 수립 
# <조건3> 변수 모델링 : x변수(contbr_occupation2), y변수(cand_pass)
# <조건4> 검정결과 해석

# 귀무가설 : 직업의 유형과 대통령 당선과 관련성이 없다.
# 대립가설 : 직업의 유형과 대통령 당선과 관련성이 있다. :  p =  2.950525e-60 < 0.05

# 변수 모델링 
x <- obama_romney2$contbr_occupation2
y <- obama_romney2$cand_pass
DF <- data.frame(x, y)

library(gmodels)
CrossTable(x=DF$x, y=DF$y, chisq = T)
# Chi^2 =  274.1462     d.f. =  2     p =  2.950525e-60 < 0.05

# [해설] 유의미한 수준에서 귀무가설을 기각할 수 있다.
# 따라서 직업의 유형과 대통령 당선과 관련성이 있다고 볼 수 있다.


## chapter 08 : lattice 패키지 ## 

# 문3) lattice 패키지의 densityplot()함수를 이용하여 후원자의 직업유형별로 후원금을 시각화하시오.
# <조건1> 대상 변수 : obama_romney2
# <조건2> 후원금이 300달러 ~ 3000달러 사이의 관측치 선정 -> obama_romney3 변수 저장  
# <조건3> obama_romney3 변수 대상 밀도그래프 시각화(x축 : 후원금, 조건 : 당선유무, 그룹 : 직업유형) 

obama_romney3 <- subset(obama_romney2, contb_receipt_amt >= 300 & contb_receipt_amt <= 3000)
range(obama_romney3$contb_receipt_amt) # 300 2975

library(lattice)
densityplot(~ contb_receipt_amt | cand_pass, data=obama_romney3, 
            groups = contbr_occupation2, plot.points=T, auto.key = T) 


## chapter 13 : 평균차이 검정 ## 

# 문4) romney와 obama 후보자를 대상으로 후원자의 직업군이 'RETIRED'인 후원금에 차이가 있는지 검정하시오.
# <조건1> 대상 변수 : obama_romney3
# <조건2> 두집단 평균차이 검정

names(obama_romney3) # cand_nm, contbr_occupation

# 1. 두 집단 서브셋 작성 
romney_retired <- subset(obama_romney3, cand_nm=='Romney, Mitt' & contbr_occupation=='RETIRED')
obama_retited <- subset(obama_romney3, cand_nm=='Obama, Barack' & contbr_occupation=='RETIRED')
romney_amt <- romney_retired$contb_receipt_amt
obama_amt <- obama_retited$contb_receipt_amt

# 2. 기술통계량 : 후보자별 후원금 평균 
mean(romney_amt) # 1383.382
mean(obama_amt) # 901.7605

# 3. 동질성 검정 
var.test(romney_amt, obama_amt)
# p-value = 8.957e-05 : 두 집단은 동질하지 않음 -> 비모수 검정 

# 4. 평균차이 검정 : 양측검정 -> 단측검정 

# 양측검정 
wilcox.test(romney_amt, obama_amt, alter='two.sided', conf.level = 0.95)
# p-value < 2.2e-16 : 평균 차이 있음 
# 양방성을 갖는 대립가설 검정  
wilcox.test(romney_amt, obama_amt, alter='greater', conf.level = 0.95) # p-value < 2.2e-16
wilcox.test(romney_amt, obama_amt, alter='less', conf.level = 0.95)#  p-value = 1

# 5. 검정 결과 해석 
# [해설] 양측검정 결과 두 집단 간에는 평균에 차이를 보인다. 또한 단측검정을 통해서 
# romney 후보자가 obama 후보자 보다 평균이 더 큰 것으로 나타남 


## chapter 15 : 분류분석 ##  

# 문5) 다음 조건에 따라서 분류분석을 수행하시오.
# <조건1> 대상 변수 : obama_romney3
# <조건2> character 자료형 칼럼  -> factor형 변환
str(obama_romney3)
obama_romney3$contbr_occupation2 <- as.factor(obama_romney3$contbr_occupation2)
obama_romney3$cand_pass <- as.factor(obama_romney3$cand_pass)
str(obama_romney3)

# <조건3> 7:3 비율로 데이터 셋 구성(train/test)
idx <- sample(1:nrow(obama_romney3), nrow(obama_romney3)*0.7)
train <- obama_romney3[idx, ]
test <- obama_romney3[-idx, ]

# <조건4> ctree()함수 이용 분류모델 생성 : train  
# <조건5> 변수 모델링 : y변수(cand_pass), x변수(contb_receipt_amt, contbr_occupation2)  

library(party)
formula = cand_pass ~ contb_receipt_amt+ contbr_occupation2
model <- ctree(formula = formula, data = train)

# <조건6> 의사결정트리 시각화 
#         -> 가장 높은 당선을 나타내는 조건식으로 서브셋 작성(당선과 낙선 수 파악)  
plot(model)
# n = 190인 경우 가장 높은 당선을 나타냄 
result <- subset(train, contb_receipt_amt <= 990 & contb_receipt_amt <= 490)
dim(result) #  190   7
table(result$cand_pass)
#낙선 당선 
# 13  177  <- 190 

# <조건7> 예측치 생성과 분류정확도 구하기 : test 
pred <- predict(model, test)
table(pred, test$cand_pass)
#pred   낙선 당선
#  낙선  112   62
#  당선  166  460

# 분류정확도 : 72%
(112 + 460) / nrow(test) # 0.715




