#################################
## <제4장 연습문제>
#################################   
# 01. 다음 조건에 맞게 client 데이터프레임을 생성하고, 조건에 맞게 처리하시오.

# vector 준비 
name <-c("유관순","홍길동","이순신","신사임당")
gender <- c("F","M","M","F")
price <-c(50,65,45,75)

# data.frame 
client <- data.frame(name,gender,price)
# <조건1> price가 65만원 이상인 고객은 "Best" 미만이면 
#     "Normal" 문자열을 result 변수에 넣고, client의 객체에 컬럼으로 추가하기

# <조건2> result의 빈도수를 구하시오. 힌트) table()함수 이용

# <조건3> gender가 'M'이면 "Male", 'F'이면 "Female" 형식으로 client의 객체에
#  gender2 컬럼을 추가하고 빈도수 구하기


# 02. 다음 벡터(EMP)는 '입사년도이름급여'순으로 사원의 정보가 기록된 데이터 있다.
# 이 벡터 데이터를 이용하여 다음과 같은 출력결과가 나타나도록 함수를 정의하시오. 

# <Vector 준비>
EMP <- c("2014홍길동220", "2002이순신300", "2010유관순260")

# <출력 결과>
# 전체 급여 평균 : 260
# 평균 이상 급여 수령자
# 이순신 => 300 
# 유관순 => 260 

#힌트) 사용 함수
#stringr 패키지 : str_extract(), str_replace()함수
#숫자변환 함수 : as.numeric()함수
#한글 문자 인식 정규표현식 : [가-히]  

# 함수 정의 : for()함수 이용 예  
emp_pay <- function(x) {  
  
}

# 함수 호출 
emp_pay(EMP)


# 03. 함수 y = f(x)에서 x의 값이 a에서 b까지 변할 때 △x = b - a를 x의 증분이라고 하며, △y = f(b) - f(a)를 y의 증분이라고 한다. 여기서 평균변화율 = △y/△x  =  f(b) - f(a)/ b-a 이다.

# 조건) 함수 f(x) = x3 + 4에서 x의 값이 1에서 3까지 변할 때 평균변화율(mean ratio of change)을 구하는 함수를 작성하시오. ∴ 평균변화율 = 31-5/2 = 13

# 04. RSADBE 패키지에서 제공되는 Bug_Metrics_Software 데이터 셋을 대상으로 소프트웨어 발표 후 행 단위 합계와 열 단위 평균을 구하고, 칼럼 단위로 요약통계량을 구하시오.  
library(RSADBE)
data("Bug_Metrics_Software")
Bug_Metrics_Software
class(Bug_Metrics_Software)
Before <- Bug_Metrics_Software[,,'Before']

After <- Bug_Metrics_Software[,,'After']
after_sum <- apply(After,1,sum)
after_mean <- apply(After,2,mean)

after_summary <- apply(After,2,summary)
