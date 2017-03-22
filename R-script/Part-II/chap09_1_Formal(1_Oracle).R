# Formal(1_Oracle)

# Formal(1_Oracle)
# Oracle DB 정형 데이터 처리

# 1. 패키지 설치
# - RJDBC 패키지를 사용하기 위해서는 우선 java를 설치해야 한다.
install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC")

# 2. 패키지 로딩
library(DBI)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_111')
library(rJava)
library(RJDBC) # rJava에 의존적이다.(rJava 먼저 로딩)

# 3) Oracle 연동   

############ Oracle 10g ##############
# driver  
drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
          "C:\\oraclexe\\app\\oracle\\product\\10.2.0\\server\\jdbc\\lib\\ojdbc14.jar")
# db연동(driver, url,uid,upwd)   
conn<-dbConnect(drv, "jdbc:oracle:thin:@//127.0.0.1:1521/xe","scott","tiger")
####################################

############ Oracle 11g ##############
# driver  
drv<-JDBC("oracle.jdbc.driver.OracleDriver", 
          "C:\\app\\acorn\\product\\11.2.0\\dbhome_1\\jdbc\\lib\\ojdbc6.jar")
# db연동(driver, url,uid,upwd)   
conn<-dbConnect(drv, "jdbc:oracle:thin:@//127.0.0.1:1521/orcl","scott","tiger")
####################################


###########################
## 1. DML 명령어 사용 
###########################
# test(sid, pwd, name, age)
query <- "select * from test"
result <- dbGetQuery(conn, query)
result

# db 내용 수정 : insert, update, delete

# 1. 레코드 추가 : 시퀀스 객체 이용 
query <- "insert into test values(seq_sid.nextval, '1234', '강감찬', 55)"
dbSendUpdate(conn, query)
dbGetQuery(conn, 'select * from test order by sid')

# 2. 레코드 수정 : sid : 1002, 홍길동 -> 김길동 
query <- "update test set name = '김길동' where sid=1002"
dbSendUpdate(conn, query)
dbGetQuery(conn, 'select * from test order by sid')

# 3. 레코드 조회 : 나이가 40세 이상 레코드 조회 
query <- "select * from test where age >= 40"
dbGetQuery(conn, query)

# 4. 레코드 삭제 : sid : 1002 레코드 삭제 
query <- "delete from test where sid=1002"
dbSendUpdate(conn, query)
dbGetQuery(conn, 'select * from test order by sid')

emp <- dbGetQuery(conn, 'select * from emp')
str(emp)
# 'data.frame':	14 obs. of  8 variables:
sal <- emp$SAL
sum(sal) #  29025
mean(sal) # 2073.214


# 문1) SAL 2500이상이고, 직책(JOB)이 MANAGER인 사원을 검색하시오.
query <- "select * from emp where sal >= 2500  and job = 'MANAGER'"
dbGetQuery(conn, query)

# 문2) SUB Query 관련 문제 
# 부서가 'SALES'인 전체 사원의 이름, 급여, 직책을 출력하시오.
# sub : DEPT 테이블, main : EMP 테이블 
dbGetQuery(conn, 'select * from DEPT')
query <- "select ename, sal, job from EMP where DEPTNO = (select DEPTNO from DEPT where DNAME='SALES')"
dbGetQuery(conn, query)

# 문3) 'WARD' 사원의 상사(MGR)와 동일한 사원이름, 직책, 연봉을 출력하시오
# sub, main : EMP 테이블 
query <-"select ename, job, sal, mgr from EMP where MGR = (select MGR from EMP where ename='WARD')"
dbGetQuery(conn, query)

# 문4) join 쿼리문 
dbGetQuery(conn, 'select * from product')
dbGetQuery(conn, 'select * from sale')

query <- "select p.code, p.name, s.price, s.sdate from product p, sale s where p.code = s.code and p.name like '%기'"
dbGetQuery(conn, query)


###########################
## 2. DB 데이터 활용  
###########################

emp <- dbGetQuery(conn, 'select * from emp')
str(emp) # 'data.frame':	14 obs. of  8 variables:
# 칼럼 선택 : 변수명$칼럼명 
emp
cat('수당의 합계 : ', sum(emp$COMM, na.rm = T)) 
# 수당의 합계 :  2200
cat('수당의 평균 : ', mean(emp$COMM, na.rm = T))
# 수당의 평균 :  550

# 문) 전체 사원의 연봉 합계, 평균 구하기 
cat('연봉의 합계 : ', sum(emp$SAL, na.rm = TRUE))
cat('연봉의 평균 : ', mean(emp$SAL, na.rm = TRUE))
cat('연봉의 표준편차 : ', sd(emp$SAL, na.rm = TRUE))
# 연봉의 표준편차 :  1182.503


# 전체 사원의 연봉 정보를 막대차트로 시각화 
help("barplot")
nrow(emp) # 14

barplot(emp$SAL, main = '전체 사원의 연봉 정보',
        col = rainbow(nrow(emp)), names.arg = emp$ENAME )

# 문) 부서별 연봉 평균과 총금액을 구하고, 막대차트로 부서별 총금액을 시각화 하시오.
query <-"select deptno, avg(sal), sum(sal) from emp group by deptno"
result <- dbGetQuery(conn, query)
result
#    DEPTNO AVG(SAL) SUM(SAL)
#1     30 1566.667     9400
#2     20 2175.000    10875
#3     10 2916.667     8750

DNAME <- c('회계', '연구', '판매')
DNAME # [1] "회계" "연구" "판매"

# 칼럼 추가 
result$DNAME <- DNAME

barplot(result$`SUM(SAL)`, main = '부서별 총금액 현황',
        col=rainbow(nrow(result)), names.arg=result$DNAME,
        xlab = '부서 번호', 
        ylab = '연봉 총금액(단위:천원)' )

# 1. 파일 가져오기 
fdata <- read.csv('c:/NCS/Rwork/Part-II/data.csv')
Date <- fdata$Date
year <- str_sub(Date, 1, 4)
month <- str_sub(Date, 5, 6)
day <- str_sub(Date, 7, 8)

# 날짜 객체 생성 
sDate <- str_c(year,month,day, sep='-')
sDate; class(sDate) # "character"
cDate <- as.Date(sDate, '%Y-%m-%d')
cDate; class(cDate) # "Date"
fdata$Date <- cDate
class(fdata$Date) # "Date"

# 2. table 생성 : 주의 : Date 칼럼명 안됨
query <- "create table db_table(Buy_Date date, Customer_ID int, Buy int)"
dbSendUpdate(conn,query)

# 3. 레코드 추가 
dbSendUpdate(conn, "insert into db_table values('2015-01-23',5,10)")
dbGetQuery(conn, 'select * from db_table')

# 3. data.frame -> table 저장 
#head(fdata) # Date Customer_ID Buy
#names(data) <- c('Buy_Date', 'Customer_ID', 'Buy'
#dbWriteTable(conn, 'db_table', fdata) # 칼럼명 -> 테이블 칼럼명 저장

# db 연결 종료
dbDisconnect(conn)


