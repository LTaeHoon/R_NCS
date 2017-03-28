################################
### Part-II 총정리
################################
election <- read.csv(file.choose()) # election_2012.csv
str(election)
# 'data.frame':	1001731 obs. of  16 variables:
# 2. cand_id : 대선후보자 id
# 3. cand_nm : 대선후보자 이름
# 4. contbr_nm : 후원자 이름 
# 5. contbr_city : 후원자 도시 
# 9. contbr_occupation : 후원자 직업군 
# 10. contb_receipt_amt: 후원금 
# 11. contb_receipt_dt:후원날짜 


# 위 7개 칼럼을 대상으로 데이터 프레임을 생성한다.
election_df2 <- election[c(2:5,9:11)]
str(election_df2) # 'data.frame':	1001731 obs. of  7 variables:
dim(election_df2) # 1001731       7
summary(election_df2) # - contbr_occupation(128 NA)


## chapter 07 ##

# 문1) 직업군 칼럼을 대상으로 NA를 포함하는 관측치를 제외하여 clean_election 변수에 저장하시오.
# <조건> 전처리 전과 후 관측치 수의 차이? 
#  힌트) subset(), is.na() 함수 이용 
x <- election_df2
clean_election <- subset(x, !is.na(x$contbr_occupation) )
dim(clean_election) # 1001603       7
1001731 - 1001603 # 128


# 문2) clean_election 변수를 대상으로 10만개 관측치만 샘플링하여 clean_election2 변수에 저장하시오.
#  힌트) sample() 함수 이용

idx <- sample(nrow(clean_election), 100000) # 1:nrow(clean_election)
clean_election2 <- clean_election[idx, ]
dim(clean_election2) # 100000      7
nrow(clean_election2) # 100000  


# 문3) 직업군의 문자열 길이가 20개을 초과한 경우 끝에서 10개 문자열로 직업군의 칼럼을 수정하는 함수를 정의하시오.
#   힌트) stringr 패키지 : str_sub(), str_length() 함수 이용
# <조건1> 대상 변수 : clean_election2 
# <조건2> 함수명 : clean_data
# <조건3> 수정된 내용으로 직업군 칼럼 수정 


clean_election2$contbr_occupation[1:10]

library(stringr)
str <-'1234567890'
str_result <- str_sub(str, str_length(str)-4, str_length(str)) # 6~10
str <- str_result
str # "67890"

# 함수 정의 
clean_data <- function(occupation){
  library(stringr)
  clean_occupation <- character() # vector 저장 변수 
  idx <- 1 # index 변수 
  
  for(data in occupation){ # 직업군 vector
    if(str_length(data) >= 20){ # 길이가 20보다 큰 경우 
      clean_occupation[idx] <- str_sub(data, str_length(data)-9, str_length(data))
      idx <- idx + 1
    }else{ # 20 미만인 경우 
      clean_occupation[idx] <- data
      idx <- idx + 1
    }
  }
  return(clean_occupation) # vector변수 반환
}

# 함수 호출 
clean_occupation <- clean_data(clean_election2$contbr_occupation)
# 리턴값으로 칼럼 수정 
clean_election2$contbr_occupation <- clean_occupation
clean_election2$contbr_occupation[1:10]


## chapter 05 ## 

# 문4) romney와 obama의 후원자 직업군 빈도수가 상위 10위 해당하는 데이터를 이용하여 시각화 하시오
# <조건1> 작업 대상 변수 : clean_election2
# <조건2> 막대차트 시각화 - 무지개 색 적용, 후원자의 직업군을 x축 눈금 이름으로 표시
# <조건3> 파이 차트 시각화 : romney와 obama 후보자 동시 표현, cex=1.2 속성 지정 
romney <- subset(clean_election2, clean_election2$cand_nm == 'Romney, Mitt')
obama <- subset(clean_election2, clean_election2$cand_nm == 'Obama, Barack')

dim(romney) # 10757     7
dim(obama) # 59245     7

head(romney); tail(romney)
head(obama); tail(obama)

x <- c(1:10)
sort(x, decreasing = T)

romney_table <- sort(table(romney$contbr_occupation), decreasing = T)
barplot(romney_table[1:10], main = 'romney 후원자 직업군',
        names.arg = names(romney_table[1:10]),
        col=rainbow(10))
# names(romney_table[1:10]) : 칼럼명 추출 

obama_table <- sort(table(obama$contbr_occupation), decreasing = T)
barplot(obama_table[1:10], main = 'obama 후원자 직업군',
        names.arg = names(obama_table[1:10]),
        col=rainbow(10))

# pie 차트 
par(mfrow=c(1,2)) # 두 화면으로 플로팅 
pie(romney_table[1:10], main = 'romney 후원자 직업군 현황', cex=1.2)
pie(obama_table[1:10], main = 'obama 후원자 직업군 현황', cex=1.2)


## chapter 07 ##
# 문5) romney와 obama 후보자 별로 다음과 같이 후원금 칼럼을 처리하시오.
# <조건1> 각 후보자별로 이상치(음수)를 발견하여 정제
# <조건2> 각 후보자별로 정제 전과 후 관측치의 차이 계산  
# <조건3> 각 후보자별로 가장 많은 후원금 찾기 

# 이상치 발견 
summary(romney$contb_receipt_amt)
summary(obama$contb_receipt_amt)
plot(romney$contb_receipt_amt)
plot(obama$contb_receipt_amt)

# 0이상 데이터 정제 
clean_romney <- subset(romney, romney$contb_receipt_amt > 0)
clean_obama <- subset(obama, obama$contb_receipt_amt > 0)

max(clean_romney$contb_receipt_amt) # 10000
max(clean_obama$contb_receipt_amt) # 5000


## chapter 09 : 정형 데이터 처리 ##
# 문6) clean_election2 변수 내용을 대상으로 다음과 같이 테이블에 저장하시오.
# <조건1> 대상 칼럼 : contb_receipt_dt, cand_nm, contbr_nm, contb_receipt_amt
# <조건2> 테이블명 : election 
# <조건3> 칼럼명(자료형) : receipt_date(date), caname(varchar(50), ctname(varchar50)), receipt_amt(int)  
# <조건4> 다국어 적용 : 후원일짜 대한민국 표준날짜형식으로 변경 후 DB 저장  
# <조건5> MariaDB 사용 
# 패키지 로딩
library(DBI)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_101')
library(rJava)
library(RJDBC) # rJava에 의존적이다.

################ MariaDB or MySql ###############
drv <- JDBC(driverClass="com.mysql.jdbc.Driver", 
            classPath="C:\\Rwork\\mysql-connector-java-5.1.39\\mysql-connector-java-5.1.39\\mysql-connector-java-5.1.39-bin.jar")

# driver가 완전히 로드된 후 db를 연결한다.
conn <- dbConnect(drv, "jdbc:mysql://127.0.0.1:3306/work", "scott", "tiger")

# Table 생성 
query <- "create table election(receipt_date date, caname varchar(50), ctname varchar(50), receipt_amt int)"
dbSendUpdate(conn, query) # table 생성 
dbListFields(conn, "election") # Table 칼럼 확인  

# data frame 구성 
DF <-  clean_election2[ c(7,2,3,6) ]
# 칼럼명 수정(table의 칼럼명과 일치)
names(DF) <- c( "receipt_date", "caname", "ctname", "receipt_amt")
head(DF)

# 날짜 칼럼 -> 한국 표준 날짜형으로 변환 
Sys.setlocale(locale = 'English_USA') # 다국어 수정 
sdate <- DF$receipt_date
cdate <- as.Date(sdate, '%d-%b-%y')
DF$receipt_date <- cdate
head(DF)

# data frame -> DB 테이블 저장 
dbWriteTable(conn, 'election', DF) # table 저장
# Table에서 obama 후보자만 레코드 검색 
obama <- dbGetQuery(conn, "select * from election where caname = 'Obama, Barack'")
head(obama)


## chapter 09 : 비정형 데이터 처리 ##
# 문7) 후원도시 칼럼을 대상으로 단어 빈도 분석을 통해서 단어구름으로 시각화하시오.
# <조건1> 대상 변수 : clean_election2$contbr_city[1:2000]
# <조건2> 토픽분석 6 ~ 10단계 적용 
# <조건3> 단어 길이 : 2 ~ 8자 대상 

library(tm) # 영문 텍스트 마이닝 
library(wordcloud) # 단어 구름 사진 


# 6. 데이터 전처리   
# (1) 추출된 단어 이용하여 자료집 생성
city <- clean_election2$contbr_city[1:2000]
myCorputfacebook <- Corpus(VectorSource(city)) 
# (2) 데이터 전처리 
myCorputfacebook <- tm_map(myCorputfacebook, removePunctuation) # 문장부호 제거
#myCorputfacebook <- tm_map(myCorputfacebook, removeNumbers) # 수치 제거
#myCorputfacebook <- tm_map(myCorputfacebook, tolower) # 소문자 변경
#myCorputfacebook <-tm_map(myCorputfacebook, removeWords, stopwords('english')) # 불용어제거
#stopwords('english')
# (3) 전처리 결과 확인 
inspect(myCorputfacebook[1:5]) # 데이터 전처리 결과 확인

# 7. 단어 선별(단어 길이 2개 이상)
# (1) 자료집 -> 일반문서 변경
myCorputfacebook_txt <- tm_map(myCorputfacebook, PlainTextDocument) 
# (2) TermDocumentMatrix() : 단어 선별(단어길이 2개 이상인 단어 선별 -> matrix 변경)
myCorputfacebook_txt <- TermDocumentMatrix(myCorputfacebook_txt, control=list(wordLengths=c(2, 8)))
myCorputfacebook_txt
# (3) matrix -> data.frame 변경(빈도분석을 위해서)
myTermfacebook.df <- as.data.frame(as.matrix(myCorputfacebook_txt)) 
dim(myTermfacebook.df) # [1] 876  76

# 8. 단어 빈도수 구하기(행 단위 합계 -> 내림차순 정렬)
wordResult <- sort(rowSums(myTermfacebook.df), decreasing=TRUE) # 빈도수로 내림차순 정렬
wordResult[1:10]
 

# 9. wordcloud 생성 (디자인 적용전)
myName <- names(wordResult) # 단어 이름 생성 -> 빈도수의 이름 
wordcloud(myName, wordResult) # 단어구름 적용


# 10. 단어구름에 디자인 적용(빈도수, 색상, 랜덤, 회전 등)
# (1) 단어이름과 빈도수로 data.frame 생성
word.df <- data.frame(word=myName, freq=wordResult) 
str(word.df) # word, freq 변수

# (2) 단어 색상과 글꼴 지정
pal <- brewer.pal(12,"Paired") # 12가지 색상 pal <- brewer.pal(9,"Set1") # Set1~ Set3
# 폰트 설정세팅 : "맑은 고딕", "서울남산체 B"
windowsFonts(malgun=windowsFont("맑은 고딕"))  #windows

# (3) 단어 구름 시각화 - 별도의 창에 색상, 빈도수, 글꼴, 회전 등의 속성을 적용하여 
x11( ) # 별도의 창을 띄우는 함수
wordcloud(word.df$word, word.df$freq, 
          scale=c(5,1), min.freq=3, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")
#wordcloud(단어, 빈도수, 5:1비율 크기,최소빈도수,랜덤순서,랜덤색상, 회전비율, 색상(파렛트),컬러,글꼴 )

