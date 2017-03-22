#################################
## <제9장 연습문제>
################################# 

# 01. 다음과 같은 단계를 통해서 테이블을 생성하고, SQL문을 이용하여 레코드를 조회하시오.(DBMS는 자유 선택)

# [단계 1] 상품정보(GoodsInfo)테이블 생성(SQLPLUS 이용)

# [단계 2] 레코드 추가(SQLPLUS 이용)

# [단계 3] 전체 레코드 검색(R 소스 코드 이용)

# 02. 공공데이터 사이트에서 관심분야 데이터 셋을 다운로드 받아서 빈도수가 5회 이상 단어를 이용하여 
#      단어 구름으로 시각화 하시오.
install.packages("rJava")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
library(rJava) # 로딩

install.packages(c("KoNLP", "wordcloud"))
remove.packages('tm')
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos=NULL)
# 4) 패키지 로딩
library(KoNLP) #세종사전, 8만여개의 단어 제공 
library(tm) # 영문 텍스트 마이닝
library(wordcloud) # RColorBrewer()함수 제공

sewol<-file("C:/NCS/Rwork/R-script/Part-II/sewol.txt", encoding="UTF-8")
sewol_data <- readLines(sewol)

#Corpus : 벡터 대상 자료집(documents)생성 함수, tm 패키지 제공
sewol_corpus <- Corpus(VectorSource(sewol_data))

useSejongDic() # 세종 사전 불러오기

#기존에 없는 단어 추가하는 함수 mergeUserDic
#mergeUserDic(data.frame(c("R 프로그래밍","페이스북","소셜네트워크"), c("ncn"))) 
# ncn -명사지시코드

# 4) 단어추출 사용자 함수 정의
# (1) 사용자 정의 함수 실행 순서 : 문자변환 -> 명사 단어추출 -> 공백으로 합침
exNouns <- function(x) { paste(extractNoun(as.character(x)), collapse=" ")}
# (2) exNouns 함수(KoNLP에서 제공 ) 이용 단어 추출 
# 형식) sapply(적용 데이터, 적용함수) -> 요약 100개를 대상으로 단어 추출
sewol_nouns <- sapply(sewol_corpus, exNouns) 
# (3) 단어 추출 결과
#class(facebook_nouns) # [1] "character" -> Vector 타입
sewol_nouns[1] # 단어만 추출된 첫 줄 보기 

# 5) 데이터 전처리   
# (1) 추출된 단어 이용 자료집 생성
# 이전 단계에서 sapply를 사용하여 vector가 반환 되었기 때문에 다시 corpus 형태로 만들어줘야 함

myCorputsewol <- Corpus(VectorSource(sewol_nouns)) 
myCorputsewol # Content:  documents: 76
inspect(myCorputsewol)
# (2) 데이터 전처리 
myCorputsewol <- tm_map(myCorputsewol, removePunctuation) # 문장부호 제거
myCorputsewol <- tm_map(myCorputsewol, removeNumbers) # 수치 제거
myCorputsewol <- tm_map(myCorputsewol, tolower) # 소문자 변경
myStopwords = c(stopwords('english'), "것\n", "들이","해\n","이번");
myCorputsewol <-tm_map(myCorputsewol, removeWords, myStopwords)# 불용어제거
?stopwords #불용어

# (3) 전처리 결과 확인 
inspect(myCorputsewol[1:5]) # 데이터 전처리 결과 확인

# 6) 단어 선별(단어 길이 2개 이상)
# PlainTextDocument 함수를 이용하여 myCorpus를 일반문서로 변경
myCorputsewol_txt <- tm_map(myCorputsewol, PlainTextDocument)
myCorputsewol_txt
inspect(myCorputsewol_txt)



# TermDocumentMatrix() : 일반텍스트문서를 대상으로 단어 선별
# 단어길이 2개 이상인 단어만 선별 -> matrix 변경
myCorputsewol_txt <- TermDocumentMatrix(myCorputsewol_txt, control=list(wordLengths=c(4,Inf)))
myCorputsewol_txt

#<<TermDocumentMatrix (terms: 876, documents: 76)>>

# matrix -> data.frame 변경
myCorputsewol.df <- as.data.frame(as.matrix(myCorputsewol_txt)) 
dim(myCorputsewol.df) # [1] 876  76

# 7) 단어 빈도수 구하기
wordResult <- sort(rowSums(myCorputsewol.df), decreasing=TRUE) # 빈도수로 내림차순 정렬
wordResult[1:10]

# 8) wordcloud 생성 (디자인 적용전)
myName <- names(wordResult) # 단어 이름 생성 -> 빈도수의 이름 
wordcloud(myName, wordResult) # 단어구름 적성

word.df <- data.frame(word=myName, freq=wordResult) 
str(word.df) # word, freq 변수

# (2) 단어 색상과 글꼴 지정
pal <- brewer.pal(12,"Paired") # 12가지 색상 pal <- brewer.pal(9,"Set1") # Set1~ Set3
# 폰트 설정세팅 : "맑은 고딕", "서울남산체 B"
windowsFonts(malgun=windowsFont("맑은 고딕"))  #windows

# (3) 단어 구름 시각화 - 별도의 창에 색상, 빈도수, 글꼴, 회전 등의 속성을 적용하여 
x11( ) # 별도의 창을 띄우는 함수
wordcloud(word.df$word, word.df$freq, 
          scale=c(5,1), min.freq=5, random.order=F, 
          rot.per=.1, colors=pal, family="malgun")
