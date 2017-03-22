#################################
## <제1장 연습문제>
#################################

#01. 현재 작업 공간을 확인하고 "C:/Rwork/Part-I"으로 변경하시오.


#02. 다음 조건에 맞게 name, age, address 변수를 생성하고 처리하시오.

#조건1) 각 변수의 특성에 맞게 값을 초기화하고 결과를 확인한다.
name <-'이태훈'
age <-32
address <- '관악구 봉천동'  

#조건2) 각 변수를 대상으로 자료형(data type)을 확인한다.
is.character(address)
is.numeric(age)
is.character(name)   

#03. R에서 제공하는 women 데이터 셋을 다음과 같이 처리하시오.

#조건1) women 데이터 셋은 어떤 데이터의 모음인가?
data("women")
summary(women)
str(women)
#조건2) women 데이터 셋의 자료형과 자료구조는?
mode(women)
class(women)
names(women) <- c('weight','height')
#조건3) plot() 함수를 이용하여 기본 차트 그리기
plot(women)

#04. R에서 제공하는 c()함수를 이용하여 벡터를 생성하고, 데이터를 처리하시오.

#조건1) 1~100까지 벡터를 생성한다.
num <- c(1:100)

#조건2) 생성된 벡터를 대상으로 평균을 구한다.
mean(num)

#05. R 프로그래밍 언어의 특징을 2가지만 기술하시오.
