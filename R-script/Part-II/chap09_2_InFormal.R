# chap09_2_InFormal

########################################
## Chapter09-2. 비정형데이터 처리 
########################################

# 분석 절차
# 1단계 : 토픽분석(단어의 빈도수)
#     데이터전처리(조사제거) -> DTM (희소행렬 만들기)-> TDM (전치행렬) -> rowSums()
# 2단계 : 연관어 분석(관련 단어 분석) 
# 단계3 - 감성 분석(단어의 긍정/부정 분석) 

# 패키지 설치 및 로딩
# 1) java install : http://www.oracle.com
# -> java 프로그램 설치(64비트 환경 - R(64bit) - java(64bit))

# 2) rJava 설치 : R에서 java 사용을 위한 패키지
install.packages("rJava")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
library(rJava) # 로딩

# 3) install.packages
install.packages(c("KoNLP", "wordcloud"))
remove.packages('tm')
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos=NULL)
# 4) 패키지 로딩
library(KoNLP) #세종사전, 8만여개의 단어 제공 
library(tm) # 영문 텍스트 마이닝
library(wordcloud) # RColorBrewer()함수 제공

##################################################
# 단계1 - 토픽분석(텍스트 마이닝) 
#- 시각화 : 단어 빈도수에 따른 워드 클라우드
###################################################

# 1) facebook_bigdata.txt 가져오기
facebook <- file("C:/NCS/Rwork/Part-II/facebook_bigdata.txt", encoding="UTF-8")
facebook_data <- readLines(facebook) # 줄 단위 데이터 생성

head(facebook_data) # 앞부분 6줄 보기 - 줄 단위 문장 확인 
str(facebook_data) # chr [1:76]

# # 2) Corpus : 벡터 대상 자료집(documents)생성 함수, tm 패키지 제공
# facebook_corpus <- Corpus(VectorSource(facebook_data))
# facebook_corpus 
# inspect(facebook_corpus) # 76개 자료집에 포함된 문자 수 제공 
# corpus 객체로 변환 안해도됨 corpus 객체 사용 이유는 tm 패키지에서 제공하는 함수 사용시 입력 값으로 사용됨
# 3) 세종 사전 사용 및 단어 추가
useSejongDic() # 세종 사전 불러오기

#기존에 없는 단어 추가하는 함수 mergeUserDic
mergeUserDic(data.frame(c("R 프로그래밍","페이스북","소셜네트워크"), c("ncn"))) 
# ncn -명사지시코드

paste(extractNoun('우리나라 대한민국 나는 홍길동 입니다.'),collapse=" ")
# [1] "우리나라" "대한"     "민국"     "나"       "홍길동"  
# [1] "우리나라 대한 민국 나 홍길동"

# 4) 단어추출 사용자 함수 정의
# (1) 사용자 정의 함수 실행 순서 : 문자변환 -> 명사 단어추출 -> 공백으로 합침
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse=" ")}
# (2) exNouns 함수(KoNLP에서 제공 ) 이용 단어 추출 
# 형식) sapply(적용 데이터, 적용함수) -> 요약 100개를 대상으로 단어 추출
facebook_nouns <- sapply(facebook_corpus, exNouns) 
# (3) 단어 추출 결과
#class(facebook_nouns) # [1] "character" -> Vector 타입
facebook_nouns[1] # 단어만 추출된 첫 줄 보기 

# 5) 데이터 전처리   
# (1) 추출된 단어 이용 자료집 생성
# 이전 단계에서 sapply를 사용하여 vector가 반환 되었기 때문에 다시 corpus 형태로 만들어줘야 함

myCorputfacebook <- Corpus(VectorSource(facebook_nouns)) 
myCorputfacebook # Content:  documents: 76
inspect(myCorputfacebook)
# (2) 데이터 전처리 
myCorputfacebook <- tm_map(myCorputfacebook, removePunctuation) # 문장부호 제거
myCorputfacebook <- tm_map(myCorputfacebook, removeNumbers) # 수치 제거
myCorputfacebook <- tm_map(myCorputfacebook, tolower) # 소문자 변경
myCorputfacebook <-tm_map(myCorputfacebook, removeWords, stopwords('english')) # 불용어제거

?stopwords #불용어

# (3) 전처리 결과 확인 
inspect(myCorputfacebook[1:5]) # 데이터 전처리 결과 확인

# 6) 단어 선별(단어 길이 2개 이상)
# PlainTextDocument 함수를 이용하여 myCorpus를 일반문서로 변경
myCorputfacebook_txt <- tm_map(myCorputfacebook, PlainTextDocument)
myCorputfacebook_txt
inspect(myCorputfacebook_txt)



# TermDocumentMatrix() : 일반텍스트문서를 대상으로 단어 선별
# 단어길이 2개 이상인 단어만 선별 -> matrix 변경
myCorputfacebook_txt <- TermDocumentMatrix(myCorputfacebook_txt, control=list(wordLengths=c(2,Inf)))
myCorputfacebook_txt

#<<TermDocumentMatrix (terms: 876, documents: 76)>>

# matrix -> data.frame 변경
myTermfacebook.df <- as.data.frame(as.matrix(myCorputfacebook_txt)) 
dim(myTermfacebook.df) # [1] 876  76

# 7) 단어 빈도수 구하기
wordResult <- sort(rowSums(myTermfacebook.df), decreasing=TRUE) # 빈도수로 내림차순 정렬
wordResult[1:10]

# 8) wordcloud 생성 (디자인 적용전)
myName <- names(wordResult) # 단어 이름 생성 -> 빈도수의 이름 
wordcloud(myName, wordResult) # 단어구름 적성


#-----------<불필요한 단어 제거 시작>-------------------
# 5) 데이터 전처리 - "사용", "하기" 단어 제거 
myCorputfacebook <- tm_map(myCorputfacebook, removePunctuation) # 문장부호 제거
myCorputfacebook <- tm_map(myCorputfacebook, removeNumbers) # 수치 제거
myCorputfacebook <- tm_map(myCorputfacebook, tolower) # 소문자 변경
myStopwords = c(stopwords('english'), "사용", "하기");
myCorputfacebook = tm_map(myCorputfacebook, removeWords, myStopwords);

inspect(myCorputfacebook[1:5]) # 데이터 전처리 결과 확인

# 6) 단어 선별(단어 길이 2개 이상)
# PlainTextDocument 함수를 이용하여 myCorpus를 일반문서로 변경
myCorputfacebook_txt <- tm_map(myCorputfacebook, PlainTextDocument) 
myCorputfacebook_txt

# TermDocumentMatrix() : 일반텍스트문서를 대상으로 단어 선별
# 단어길이 2개 이상인 단어만 선별 -> matrix 변경

myCorputfacebook_txt <- TermDocumentMatrix(myCorputfacebook_txt, control=list(wordLengths=c(2,Inf)))
myCorputfacebook_txt
#<<TermDocumentMatrix (terms: 876, documents: 76)>>

# matrix -> data.frame 변경
myTermfacebook.df <- as.data.frame(as.matrix(myCorputfacebook_txt)) 
dim(myTermfacebook.df) # [1] 876  76
View(myTermfacebook.df)
# 7) 단어 빈도수 구하기
wordResult <- sort(rowSums(myTermfacebook.df), decreasing=TRUE) # 빈도수로 내림차순 정렬
names(wordResult)
str(wordResult)
wordResult[1:10]
#빅데이터     분석     처리     수집     결과 
#21       19       16       15       13  

# 8) 단어 구름(wordcloud) 생성  생성 - 디자인 적용 전
myName <- names(wordResult) # 단어 이름 추출(빈도수 이름) 
wordcloud(myName, wordResult) # 단어구름 시각화 
#myName
#-----------<불필요한 단어 제거 종료 >-------------------

# 9) 단어구름에 디자인 적용(빈도수, 색상, 랜덤, 회전 등)
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

# 10) 차트 시각화 - 서진수(2011), R까지, 느린 생각  
#(1) 상위 10개 토픽추출
topWord <- head(sort(wordResult, decreasing=T), 10) # 상위 10개 토픽추출 
# (2) 파일 차트 생성 
pie(topWord, col=rainbow(10), radius=1) # 파이 차트-무지개색, 원크기
# (3) 빈도수 백분율 적용 
pct <- round(topWord/sum(topWord)*100, 1) # 백분율
names(topWord)
# (4) 단어와 백분율 하나로 합친다.
lab <- paste(names(topWord), "\n", pct, "%")
# (5) 파이차트에 단어와 백분율을 레이블로 적용 
pie(topWord, main="SNS 빅데이터 관련 토픽분석", 
    col=rainbow(10), cex=0.8, labels=lab)



###########################################
# 단계2 - 연관어 분석(단어와 단어 사이 연관성 분석) 
#   - 시각화 : 연관어 네트워크 시각화,
###########################################

# 한글 처리를 위한 패키지 설치
install.packages("rJava")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
library(rJava) # 아래와 같은 Error 발생 시 Sys.setenv()함수로 java 경로 지정

install.packages("KoNLP")
library(KoNLP) # rJava 라이브러리가 필요함


# 1.텍스트 파일 가져오기
#----------------------------------------------------
marketing <- file("C:/NCS/Rwork/Part-II/marketing.txt", encoding="UTF-8")
marketing2 <- readLines(marketing) # 줄 단위 데이터 생성
# incomplete final line found on - Error 발생 시 UTF-8 인코딩 방식으로 재 저장
close(marketing) 
head(marketing2) # 앞부분 6줄 보기 - 줄 단위 문장 확인 
str(marketing2)
#----------------------------------------------------

# 2. 줄 단위 단어 추출
#----------------------------------------------------
# Map()함수 이용 줄 단위 단어 추출 : Map(f, ...) -> base)
lword <- Map(extractNoun, marketing2) 
length(lword) # [1] 472
lword <- unique(lword) # 중복제거1(전체 대상)
length(lword) # [1] 353(19개 제거)

lword <- sapply(lword, unique) # 중복제거2(줄 단위 대상) 
length(lword) # [1] 352(1개 제거)
str(lword) # List of 353
lword # 추출 단어 확인
#----------------------------------------------------

# 3. 전처리
#----------------------------------------------------
# 1) 길이가 2~4 사이의 단어 필터링 함수 정의
filter1 <- function(x){
  nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)
}
# 2) Filter(f,x) -> filter1() 함수를 적용하여 x 벡터 단위 필터링 
filter2 <- function(x){
  Filter(filter1, x)
}
# is.hangul() : KoNLP 패키지 제공
# Filter(f, x) : base
# nchar() : base -> 문자 수 반환
?Filter
# 3) 줄 단어 대상 필터링
lword <- sapply(lword, filter2)

lword # 추출 단어 확인(길이 1개 단어 삭제됨)
#----------------------------------------------


# 4. 트랜잭션 생성 : 연관분석을 위해서 단어를 트랜잭션으로 변환
#----------------------------------------------------
# arules 패키지 설치
install.packages("arules")
library(arules) 
#--------------------
# arules 패키지 제공 기능
# - Adult,Groceries 데이터 셋
# - as(),apriori(),inspect(),labels(),crossTable()
#-------------------
wordtran <- as(lword, "transactions") # lword에 중복데이터가 있으면 error발생
wordtran 

# 트랜잭션 내용 보기 -> 각 트랜잭션의 단어 보기
inspect(wordtran)  

# 동일한 단어끼리 교차테이블 작성 - 내용확인 코드 
# wordtable <- crossTable(wordtran) # 교차표 작성
# length(wordtable) # 5890329
# str(wordtable) # [1:2427, 1:2427] 

# 5.단어 간 연관규칙 산출
#----------------------------------------------------
# 트랜잭션 데이터를 대상으로 지지도와 신뢰도를 적용하여 연관규칙 생성
# 형식) apriori(data, parameter = NULL, appearance = NULL, control = NULL)

tranrules <- apriori(wordtran, parameter=list(supp=0.25, conf=0.05)) 
inspect(tranrules) # 연관규칙 생성 결과(59개) 보기


# 6.연관어 시각화 - rulemat[c(34:63),] # 연관규칙 결과- {} 제거(1~33)

# (1) 데이터 구조 변경 : 연관규칙 결과 -> 행렬구조 변경(matrix 또는 data.frame) 
rules <- labels(tranrules, ruleSep=" ")  
rules
class(rules)

# 문자열로 묶인 연관단어를 행렬구조 변경 
rules <- sapply(rules, strsplit, " ",  USE.NAMES=F) 
rules
class(rules) # [1] "list"
# 행 단위로 묶어서 matrix로 반환
rulemat <- do.call("rbind", rules)
rulemat
class(rulemat)

# (2) 연관어 시각화를 위한 igraph 패키지 설치
install.packages("igraph") # graph.edgelist(), plot.igraph(), closeness() 함수 제공
library(igraph)   

# (3) edgelist보기 - 연관단어를 정점 형태의 목록 제공 
ruleg <- graph.edgelist(rulemat[c(12:59),], directed=F) # [1,]~[11,] "{}" 제외
help(graph.edgelist)
ruleg

# (4) edgelist 시각화
X11()
plot.igraph(ruleg, vertex.label=V(ruleg)$name,
            vertex.label.cex=1.2, vertex.label.color='black', 
            vertex.size=20, vertex.color='green', vertex.frame.color='blue')

#############################################
# 단계3 - 감성 분석(단어의 긍정/부정 분석) 
#  - 시각화 : 파랑/빨강 -> 불만고객 시각화
#############################################

# 1) 데이터 가져오기("c:/Rwork/Part-II/reviews.csv") 
data<-read.csv(file.choose()) # file.choose() 파일 선택
head(data,2)
dim(data) # 100   2
str(data) # 변수명 : company, review <- 고객 인터뷰 내용

# 2) 단어 사전에 단어추가

# 긍정단어와 부정단어를 카운터하여 긍정/부정 형태로 빈도 분석
#neg.txt : 부정어 사전
#pos.txt : 긍정어 사전

# (1) 긍정어/부정어 영어 사전 가져오기
setwd("C:/NCS/Rwork/Part-II")
posDic <- readLines("posDic.txt")
negDic <- readLines("negDic.txt")
length(posDic) # 2006
length(negDic) # 4783


# (2) 긍정어/부정어 단어 추가 
posDic.final <-c(posDic, 'victor')
negDic.final <-c(negDic, 'vanquished')
# 마지막에 단어 추가
posDic.final; 
negDic.final 


# 3) 감성 분석 함수 정의-sentimental

# (1) 문자열 처리를 위한 패키지 로딩 
library(plyr) # laply()함수 제공
library(stringr) # str_split()함수 제공

a<-list(a=1:5)
a
laply(a,function(x,y,z){
  x+y+z
},2,3)

# 1  2  3  4  5 
# 6  7  8  9 10 <- x + y + z 

# (2) 감성분석을 위한 함수 정의
sentimental = function(sentences, posDic, negDic){
  
  scores = laply(sentences, function(sentence, posDic, negDic) {
    
    sentence = gsub('[[:punct:]]', '', sentence) #문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) #특수문자 제거
    sentence = gsub('\\d+', '', sentence) # 숫자 제거
    sentence = tolower(sentence) # 모두 소문자로 변경(단어가 모두 소문자 임)
    
    word.list = str_split(sentence, '\\s+') # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상) 
    words = unlist(word.list) # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, posDic) # words의 단어를 posDic에서 matching
    neg.matches = match(words, negDic)
    
    pos.matches = !is.na(pos.matches) # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # 긍정 - 부정    
    return(score)
  }, posDic, negDic)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# 4) 감성 분석 : 두번째 변수(review) 전체 레코드 대상 감성분석
result<-sentimental(data[,2], posDic.final, negDic.final)
result
names(result) # "score" "text" 
dim(result) # 100   2
result$text
result$score # 100 줄 단위로 긍정어/부정어 사전을 적용한 점수 합계

# score값을 대상으로 color 칼럼 추가
result$color[result$score >=1] <- "blue"
result$color[result$score ==0] <- "green"
result$color[result$score < 0] <- "red"

# 감성분석 결과 차트보기
plot(result$score, col=result$color) # 산포도 색생 적용
barplot(result$score, col=result$color, main ="감성분석 결과화면") # 막대차트


# 5) 단어의 긍정/부정 분석 

# (1) 감성분석 빈도수 
table(result$color)

# (2) score 칼럼 리코딩 
result$remark[result$score >=1] <- "긍정"
result$remark[result$score ==0] <- "중립"
result$remark[result$score < 0] <- "부정"

sentiment_result<- table(result$remark)
sentiment_result

# (3) 제목, 색상, 원크기
pie(sentiment_result, main="감성분석 결과", 
    col=c("blue","red","green"), radius=0.8) # ->  1.2

