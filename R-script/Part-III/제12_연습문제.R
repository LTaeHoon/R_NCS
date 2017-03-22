#################################
## <제12장 연습문제>
################################# 

# 01. 교육수준(education)과 흡연율(smoking) 간의 관련성을 분석하기 위한 연구가설을 수립하고, 
# 이를 토대로 가설을 검정하시오.[독립성 검정]

#귀무가설 : 교육수준과 흡연율간의 관련성이 없다.
#연구가설 : 교육수준과 흡연율간의 관련성이 있다.

#<단계 1> 파일 가져오기
setwd("c:/NCS/Rwork/Part-III")
smoke <- read.csv("smoke.csv", header=TRUE)
# 변수 보기
head(smoke) # education, smoking 변수
names(smoke)
str(smoke)
#<단계 2> 코딩 변경 - 변수 리코딩 <- 가독성 제공    
# education(독립변수) : 1:대졸, 2:고졸, 3:중졸 
# smoking(종속변수): 1:과다흡연, 2:보통흡연, 3:비흡연
smoke$education1[smoke$education==1] <- '대졸'
smoke$education1[smoke$education==2] <- '고졸'
smoke$education1[smoke$education==3] <- '중졸'

smoke$smoking1[smoke$smoking==1] <- '과다흡연'
smoke$smoking1[smoke$smoking==2] <- '보통흡연'
smoke$smoking1[smoke$smoking==3] <- '비흡연'

#<단계 3> 교차분할표 작성    
CrossTable(x=smoke$education1,y=smoke$smoking1,chisq = T)

#<단계 4> 독립성 검정
# Chi^2 =  18.91092 > 9.48773 귀무가설 기각     d.f. =  4     p =  0.0008182573
# p-value : 0.0008182573 <0.05 귀무가설 기각 

# <단계 5> 검정결과 해석
# 교육수준과 흡연율간의 관련성이 있다.

?chisq.test
# 02. 나이(age3)와 직위(position) 간의 관련성을 단계별로 분석하시오. [독립성 검정]
#[단계 1] 파일 가져오기
setwd("c:/Rwork/Part-III")
data <- read.csv("cleanData.csv", header=TRUE)
head(data)

#[단계 2] 코딩 변경(변수 리코딩)  

#[단계 3] 산점도를 이용한 변수간의 관련성 보기 - plot(x,y) 함수 이용

#[단계 4] 독립성 검정

#[단계 5] 검정결과 해석


# 03. 직업유형에 따른 응답정도에 차이가 있는가를 단계별로 검정하시오.[동질성 검정]

#[단계 1] 패키지 설치 및 로딩
install.packages("XLConnect")    
library(XLConnect)     

#[단계 2] 파일 가져오기
setwd("c:/NCS/Rwork/Part-III")
response <- read.csv("response.csv", header=TRUE)
head(response) # 변수 보기

# [단계 3] 코딩 변경 
# job 칼럼 코딩 변경 : 1:학생, 2:직장인, 3:주부 
# response 칼럼 코딩 변경 : 1:무응답, 2:낮음, 3:높음
response$job2[response$job==1] <-'학생'
response$job2[response$job==2] <-'직장인'
response$job2[response$job==3] <-'주부'

response$response2[response$response==1] <-'무응답'
response$response2[response$response==2] <-'낮음'
response$response2[response$response==3] <-'높음'

# [단계 4] 교차분할표 작성
CrossTable(response$job2,response$response2,chisq = T)
# [단계 5] 동일성 검정  
#p =  6.900771e-12 < 0.05 귀무가설 기각
# Chi^2 =  58.2081 > 9.48773 귀무가설 기각
#[단계 6] 검정결과 해석
#직업유형에 따른 응답정도에 차이가 있다.
