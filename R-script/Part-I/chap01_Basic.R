# chap01_Basic

# 수업내용
# 1. 패키지와 session 보기
# 2. 패키지 사용법
# 3. 변수와 데이터 유형
# 4. 기본함수 사용 및 작업공간

# 1. 패키지와 session 보기
pack<-available.packages()
class(pack)
dim(pack)
#[1] 10092(행: 패키지 수)    17(열)

sessionInfo()
# version, 다국어 정보, 설치된 패키지

# 주요 단축키
# script 실행 : Ctrl + Enter, Ctrl + R
# 자동완성 : Ctrl + space
# 저장 : Ctrl + s

# R 실행방법 2가지
# 1) 줄 단위 실행
a <- rnorm(20) # n=20
hist(a)
mean(a)
# 2) 블럭 단위 실행
pdf("c:/NCS/Rwork/output/test.pdf") #open
hist(a)
dev.off() # close

# 2. 패키지 사용법
# 패키지 = function(알고리즘) + data set -> 꾸러미

# 1) 패키지 설치
install.packages("stringr") #"패키지명"
library(stringr) # 메모리 로딩

str <- "홍길동35이순신45유관순25"
#str 대상 이름 추출
str_extract_all(str,"[가-히]{3}")
#[1] "홍길동" "이순신" "유관순"
#str 대상 숫자 추출
num<-str_extract_all(str,"[0-9]{2,}")
# [1] "35" "45" "25"
#형변환
lapply(num,as.numeric)

########################
##설치 Error 해결방법
########################
# 1. 최초 install : R 관리자 모드로 실행하여 설치
# 2.기존 version -> new version
#  1) 패키지 제거
#  2) 컴퓨터 rebooting
#  3) 패키지 설치

# 2) 패키지 삭제
remove.packages("stringr")
# 방법2) 물리적 폴더 삭제

# 3) 기본 패키지/ 데이터 셋
# sessionInfo() : 패키지 확인(7)
data() # 데이터셋 확인 용도
data("Nile")
Nile
# Time Series:
#   Start = 1871 
# End = 1970 
# Frequency = 1 
length(Nile)
plot(Nile)
hist(Nile)

# 3. 변수와 데이터 유형

# (1) 변수 : 메모리(휘발성) 이름

# (2) 변수 작성 규칙
# - 첫자는 영문자, 두번째는 숫자, 특수문자(_, .)
# - 예약어 사용 불가
# - 대소문 구분(num != NUM)
# - 변수 선언 시 type 없음 : 할당된 값에 의해서 자동으로 결정
#    -> [int] num = 10;
# - 특징 : 가장 최근값만 저장
# - 특징 : R의 모든 변수는 객체(참조변수) = Python 도 동일


#[실습]
var1 <- 0  # var=10 (if(var ==10)) 에서 == 이 사용됨 그래서 변수 선언 시에 헷갈리지 않도록 <-를 권고
var1 <- 1
var1
#[1] 1

var2 <- 10
var3 <- 20
var1;var2;var3

# 변수명.멤버 cf) 객체.멤버
member.id <- 'hong'
member.pwd <- '1234'
member.name <- '홍길동'
member.age <- 35

member.id;member.age

# scalar vs vector 변수
age <- 35 #scalar 변수 : 원소 1개
name <- '홍길동'
age;name

#vector 변수
age <- c(35,45,22)
name <- c('홍길동','이순신','유관순')
age;name
#[1]"홍길동" "이순신" "유관순"
# cf)java, python : 0
# R index : 1부터 시작

name[2] # "이순신"

#data structure : chap02 참
#scalar(0차원) -> vector(1차원) -> matrix(2차원) -> array(3차원)

# 패키지/변수 목록 보기
search() #패키지 목록 
ls() # 변수 목록

# (3) 자료형(data type) : p.26
# - 숫자형, 문자형, 논리형

int <- 100 # 숫자형
string <- "대한민국" # 문자형
boolean <- TRUE # T or FALSE(F)
int;string;boolean

# (a) 자료형 보기 함수 : mode(), is.xxx()
mode(int);mode(string);mode(boolean)
#is.xxx()
is.numeric(int)
is.character(string)
is.logical(boolean)

score <- c(85,95,NA,75,65)
score
mean(score,na.rm=T)
sum(score,na.rm=T)
mode(score) #"numeric"
class(score) #"numeric"
############################
#####<연습문제2>
############################

# (4) 자료형 변환

# (a) 문자열 -> 숫자형(연산, plotting)
x <- c(10,20,30,"40")
mode(x) #"numeric"
mode(x) #"character"
x #"10" "20" "30" "40"
num <-as.numeric(x) # 문자형  -> 숫자형 변환
sum(num)
plot(num)

#(b) 요인형(Factor) 
# - 동일한 값을 범주로 갖는 vector 자료

gender <-c('M','F','F','M','M')
gender #"M" "F" "F" "M" "M"
plot(gender)
fgender <- as.factor(gender)
plot(fgender)
fgender
# [1] M F F M M
# Levels: F M
str(fgender)
# Factor w/ 2 levels "F","M": 2 1 1 2 2

#요인형 순서 변경
x <- c('M','F') # M=1, F=2
fgender2 <- factor(gender,levels = x)
fgender2
plot(fgender2)

# mode() vs class()
# mode() : data type 리턴
# class() : 자료구조 리턴

class(gender) #"character"
class(fgender) #"factor"

# Factor 자료형 고려사항
x <- c(4,2,4,2)

# 1. 숫자 -> 요인
f <- as.factor(x) #numeric -> factor
f
# Levels: 2 4
#num <- as.numeric(f)
#num #2 1 2 1

# 숫자 -> 요인 -> 숫자

# 2. 요인 -> 문자형
str <- as.character(f)
# 3. 문자형 -> 숫자형
num <- as.numeric(str)
num

# (c) 날짜형 변환
Sys.Date() #"2017-02-27"
Sys.time() #"2017-02-27 12:47:07 KST"

# (1) 문자열 -> 날짜형
today <- '2017-02-27 12:47:39'
mode(today) #"character"
class(today) #"character"

ctoday <- strptime(today,format="%Y-%m-%d %H:%M:%S")
mode(ctoday) #"list"
class(ctoday) # "POSIXlt" "POSIXt"

# (2) 영어식 날짜 -> 한국식 날짜
sdate <- '20-Jun-17' #2017-06-20

# 다국어 정보 : 한국 -> 영어
Sys.getlocale() #LC_COLLATE=Korean_Korea
Sys.setlocale(locale = 'English_USA') # 미국식

ktoday <- strptime(sdate,format="%d-%b-%y")
ktoday # "2017-06-20 KST"

Sys.setlocale(locale='Korean_korea')

# strptime() vs as.Date()
# strptime() : 날짜/시간 객체 생성
# as.Date() : 날짜 객체 생성
today2 <-as.Date(today,"%Y-%m-%d")
class(today2) # "Date"

data <- c('02/28/17', '02/29/17','03/01/17')
as.Date(data,'%m/%d/%y')
# 입력 : vector -> 출력 : vector

# 4. 기본함수 사용 및 작업공간
help(as.Date)
install.packages("stringr")
library(stringr)
help("str_extract")
?sum #sum(..., na.rm = FALSE)

args(sum)

example(sum)
d <- c(10,25)
sum(d)
sum(10,20)
c <- 1:10
mean(c,trim=0.2) #15
mean(10,20) #10 
?mean #mean(x, ...)

#google
#stringr in r

#작업공간
getwd() #"C:/NCS/Rwork/Part-I"
setwd("C:/NCS/Rwork/Part-I")

test <- read.csv('test.csv')
test

#메모리 객체(참조변수) -> file 저장
ls()
# 메모리 객체 제거
rm(list=ls())

x <- 1:10
x   
y <- 100:200
y

setwd('c:/NCS/Rwork/output')
getwd()
save(x,y,file='xy.RData')
rm(list=ls())

#file 가져오기
load('xy.RData')
