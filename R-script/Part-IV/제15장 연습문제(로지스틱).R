###################################
# 로지스틱 회귀분석 연습문제 
###################################
# 문)  admit 객체를 대상으로 다음과 같이 로지스틱 회귀분석을 수행하시오.
# <조건1> 변수 모델링 : y변수 : admit, x변수 : gre, gpa, rank 
# <조건2> 7:3비율로 데이터셋을 구성하여 모델과 예측치 생성 
# <조건3> 분류 정확도 구하기 

# 파일 불러오기
admit <- read.csv('c:/Rwork/Part-IV/admit.csv')
str(admit) # 'data.frame':	400 obs. of  4 variables:
#$ admit: 입학여부(합격여부) - int  0 1 1 1 0 1 1 0 1 0 ...
#$ gre  : 시험점수 - int  380 660 800 640 520 760 560 400 540 700 ...
#$ gpa  : 시험점수 - num  3.61 3.67 4 3.19 2.93 3 2.98 3.08 3.39 3.92 ...
#$ rank : 학교등급 - int  3 3 1 4 4 2 1 2 3 2 ...

# 1. data set 구성 
idx <- sample(1:nrow(admit), nrow(admit)*0.7)
train_admit <- admit[idx, ]
test_admin <- admit[-idx, ]
dim(train_admit)
dim(test_admin)

# 2. model 생성 

# 3. predict 생성 

# 4. 모델 평가(분류정확도) : 혼돈 matrix 이용/ROC Curve 이용

