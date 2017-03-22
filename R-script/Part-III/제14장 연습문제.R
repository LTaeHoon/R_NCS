#################################
## <제14장 연습문제>
################################# 

# 01. 다음은 drinkig_water_example.sav 파일의 데이터 셋이 구성된 테이블이다. 
# 전체 2개의 요인에 의해서 7개의 변수로 구성되어 있다. 아래에서 제시된 각 단계에 맞게  
# 요인분석을 수행하시오.  

# 단계 1 : 데이터 파일 가져오기 
library(memisc)
setwd("C:\\NCS\\Rwork\\Part-III")
data.spss <- as.data.set(spss.system.file('drinking_water_example.sav'))
data.spss
drinkig_water_exam <- data.spss[1:7]
drinkig_water_exam_df <- as.data.frame(drinkig_water_exam) 
str(drinkig_water_exam_df)
pc<-prcomp(drinkig_water_exam_df)
plot(pc)
biplot(pc) # 주성분 시각화

name_exam <- 1:380
#단계 2 : 베리멕스 회전법, 요인수 2, 요인점수 회귀분석 방법을 적용하여 요인분석  
result_exam <- factanal(drinkig_water_exam_df, factors = 2, rotation = "varimax",
                    scores = "regression")
#단계 3 : 요인적재량 행렬의 칼럼명 변경 
result_exam$loadings 
print(result_exam,cutoff=0.5) # +- 0.5 이상인 값이 유의

fact_exam_load<-result_exam$loadings
colnames(fact_exam_load) <- c("제품친밀도","제품적절성")

#단계 4 : 요인점수를 이용한 요인적재량 시각화
# 요인점수행렬(= 관측치 수) 
plot(result_exam$scores[,c(1,2)], main="Factor1과 Factor2 요인점수 행렬")

# 관측치별 이름 매핑(rownames mapping)
text(result_exam$scores[,1], result_exam$scores[,2], 
     labels = name_exam, cex = 0.7, pos = 3, col = "blue")

# 요인적재량 plotting
points(fact_exam_load[,c(1,2)], pch=19, col = "red")
# Factor1, Factor3 요인적재량 표시  

text(fact_exam_load[,1], fact_exam_load[,2], 
     labels = rownames(fact_exam_load), 
     cex = 0.8, pos = 3, col = "red")

#단계 5 : 요인축소 : 요인분석 지표 이용 요인 축소 
exam_closeness <- numeric() #제품 친밀도
exam_pertinence <- numeric() #제품 적절성

fact_exam_load_mat<-as.matrix(fact_exam_load)  
drinkig_water_exam_mat<-as.matrix(drinkig_water_exam_df)  

result_fact<-drinkig_water_exam_mat %*% fact_exam_load_mat
result_fact<-as.data.frame(result_fact)
head(result_fact)       

# 02. 문제 01에서 생성된 두 개의 요인을 데이터프레임으로 생성한 후 이를 이용하여 
# 두 요인 간의 상관관계 계수를 제시하시오.
cor(result_fact)

