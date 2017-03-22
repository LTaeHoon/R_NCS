# chap13_Ttest_Anova(연습문제)

#############################################
# 추론통계분석 - 1-1. 단일집단 비율차이 검정
#############################################

# 01. 중소기업에서 생산한 HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 
# 기존 구매비율(15%) 보다  향상되었는지를 각 단계별로 분석을 수행하여 검정하시오.

# 연구가설(H1) : 기존 구매비율과 차이가 있다.
# 귀무가설(H0) : 기존 구매비율과 차이가 없다.
# 
# 조건) 구매여부 변수 : buy (1: 구매하지 않음, 2: 구매)
# 
# (1) 데이터셋 가져오기
setwd("c:/NCS/Rwork/Part-III")
hdtv <- read.csv("hdtv.csv", header=TRUE)

# (2) 빈도수와 비율 계산
head(hdtv)
table(hdtv$buy)
freq(hdtv$buy)

binom.test(c(10,40),p = 0.15, alternative = "two.sided", conf.level = 0.95)

# (3)가설검정
# ----------------------------------------------------------------
#p-value = 0.321 >0.05 귀무가설 채택 차이가 없다.
# 
##########################################
# 추론통계학 분석 - 1-2. 단일집단 평균차이 검정
##########################################

# 02. 우리나라 전체 중학교 2학년 여학생 평균 키가 148.5cm로 알려져 있는 상태에서 
# A중학교 2학년 전체 500명을 대상으로 10%인 50명을 표본으로 선정된 데이터 셋을 이용하여
# 모집단의 평균과 차이가 있는지를 각 단계별로 분석을 수행하여 검정하시오.

#(1) 데이터셋 가져오기
setwd("c:/NCS/Rwork/Part-III")
height<- read.csv("student_height.csv", header=TRUE)

# (2) 기술통계량 평균 계산
summary(height)
head(height)
# (3) 정규성 검정
shapiro.test(height$height)
# p-value = 0.0001853 <0.05 연구가설 채택 (정규분포가 아님)

wilcox.test(height$height,mu=148.5,alternative = "two.sided",conf.level = 0.95)
#p-value = 0.067 >0.05 귀무가설 채택
?wilcox.test
# (4) 가설검정 
# ----------------------------------------------------------------
# 우리나라 중학교 2학년 여학생 평균과 A 중학교 2학년 여학생 평균 키의 차이는 없다.

############################################
# 추론통계학 분석 - 2-1. 두집단 비율 차이 검정
############################################

# 03. 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 
# 대해서 만족도에 차이가 있는가를 검정하시오.

# 힌트) 두 집단 비율 차이 검정
#  조건) 파일명 : two_sample.csv, 변수명 : gender(1,2), survey(0,1)
# ----------------------------------------------------------------
getwd()
data<-read.csv("two_sample.csv", header = T)
head(data)
summary(data)

data<-data[,c('gender','survey')]
x<-data$gender
y <-data$survey
table(x)
table(y)
table(data)


prop.test(c(138,107),c(174,126),alternative = "two.sided",conf.level = 0.95)
#p-value = 0.2765 > 0.05 귀무가설 채택

# 남녀 두집단 간 대학 만족도 비율에 차이가 없다.

##################################################
# 추론통계학 분석 - 2-2. 두집단 평균 차이 검정
##################################################

# 04. 교육방법에 따라 시험성적에 차이가 있는지 검정하시오.

#힌트) 두 집단 평균 차이 검정
#조건1) 파일 : twomethod.csv
#조건2) 변수 : method : 교육방법, score : 시험성적
#조건3) 모델 : 교육방법(명목)  ->  시험성적(비율)
#조건4) 전처리 : 결측치 제거
# ----------------------------------------------------------------  

twotest <- read.csv('twomethod.csv',header = T)
head(twotest)
summary(twotest)

result <- subset(twotest, !is.na(score),c('method','score'))
result
length(result$method)
length(result$score)

score1 <- subset(reslut, method==1)
score1
score2 <- subset(reslut, method==2)

var.test(score1$score,score2$score)

t.test(score1$score,score2$score,alternative = "two.sided",conf.level = 0.95)
# p-value = 1.303e-06 <0.05 연구가설 채택

t.test(score1$score,score2$score,alternative = "greater",conf.level = 0.95)

# 연구가설 : 첫번째 방법 < 두번째 방법  p-value = 6.513e-07 <0.05 연구가설 채택 
t.test(score1$score,score2$score,alternative = "less",conf.level = 0.95)
# 두번째 교육방법이 성적이 더 좋다.