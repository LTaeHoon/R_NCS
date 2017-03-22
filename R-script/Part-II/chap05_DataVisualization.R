#chap05_DataVisualization
#Part-II. 탐색적 데이터분석과 전처리

# chap05_DataVisualization 수업내용
# 이산변수와 연속변수 시각화

#1. 이산변수(discrete quantitative data) 시각화
# - 정수단위로 나누어 측정할 수 있는 변수

#1) 막대차트 시각화

# (1) 세로 막대 차트

# 막대차트 데이터 생성
chart_data <- c(305,450, 320, 460, 330, 480, 380, 520) 
names(chart_data) <- c("2016 1분기","2017 1분기","2016 2분기","2017 2분기","2016 3분기","2017 3분기","2016 4분기","2017 4분기")
str(chart_data)
chart_data

# 세로 막대 차트 
barplot(chart_data, ylim=c(0,600),  
        col=rainbow(8), main ="2016년도 vs 2017년도 분기별 매출현황 비교") 
help("barplot")
barplot(chart_data, ylim=c(0,600), ylab="매출액(단위:만워)", xlab="년도별 분기현황",  
        col=rainbow(8), main ="2016년도 vs 2017년도 분기별 매출현황 비교") 

# (2) 가로 막대 차트 
barplot(chart_data, xlim=c(0,600), horiz=TRUE,  
        col=rainbow(8), main ="2016년도 vs 2017년도 분기별 매출현황 비교") 
help("barplot")
barplot(chart_data, xlim=c(0,600), horiz=TRUE, 
        xlab="매출액(단위:만워)", ylab="년도별 분기현황",  
        col=rainbow(8), 
        main ="2016년도 vs 2017년도 분기별 매출현황 비교") 
barplot(chart_data, xlim=c(0,600), horiz=TRUE, 
        xlab="매출액(단위:만워)", ylab="년도별 분기현황",  
        col=rainbow(8),space = 1, cex.names = 0.8,
        main ="2016년도 vs 2017년도 분기별 매출현황 비교") 

# red와 blue 색상 4회 반복
barplot(chart_data, xlim=c(0,600), horiz=TRUE, 
        xlab="매출액(단위:만워)", ylab="년도별 분기현황",  
        space = 2, cex.names = 0.8, col=rep(c(2, 4),4)) 

# col=rep(c(2,3),5) -> red와 green 색상 5회 반복
# 1 : 검정, 2: 빨강, 3: 초록, 4: 파랑, 5: 하늘색, 6: 자주색, 7 : 노랑색

# red와 green 색상 4회 반복
barplot(chart_data, xlim=c(0,600), horiz=TRUE, 
        xlab="매출액(단위:만워)", ylab="년도별 분기현황",  
        space = 2, cex.names = 0.8, col=rep(c("red","green"),4) ) 

data()
VADeaths

par(mfrow=c(1,2)) # 1행 2열 그래프 보기

barplot(VADeaths, beside=T,col=rainbow(5), 
        main="미국 버지니아주 하위계층 사망비율")
legend(19, 71, c("50-54","55-59","60-64","65-69","70-74"), cex=0.8, fill=rainbow(5))

barplot(VADeaths, beside=F,col=rainbow(5))
title(main ="미국 버지니아주 하위계층 사망비율",font.main=4) 
legend(3.8, 200, c("50-54","55-59","60-64","65-69","70-74"), cex=0.8, fill=rainbow(5) )

#2) 점 차트 시각화
par(mfrow=c(1,1)) # 1행 1열 그래프 보기

help(dotchart)
dotchart(chart_data, color=c("green","red"), lcolor="black", pch=1:2,
         labels=names(chart_data), xlab="매출액",
         main="분기별 판매현황 점 차트 시각화", cex=1.2)
# col=9:10 -> BR(검정), AR(빨강)
# lcolor="black" -> 구분선(line) 검정색
# pch=1:2 -> 점 모양 : 원(1), 삼각형(2), +(3) 
# labels=names(Severity_Counts) : 점의 레이블 표시
# cex=1.2 -> 1.2배 확대(character expension)

#3) 파이 차트 시각화
pie(chart_data, labels = names(chart_data), border='blue', col=rainbow(8), cex=1.2) 
title("2016~2017년도 분기별 매출현황")


#2. 연속변수(Continuous quantitative data) 시각화
# - 시간, 길이 등과 같은 연속성을 가진 변수

#1) 상자 그래프 시각화
# - 상자 그래프는 요약정보를 시각화한다.

boxplot(VADeaths, range=0) # 상자 그래프 시각화
# range=0 : 최소값과 최대값을 점선으로 연결하는 역할

boxplot(VADeaths, range=0, notch=T)
abline(h=37, lty=3, col="red") # 기준선 추가(lty=3 : 선 스타일-점선)

summary(VADeaths)

#2) 히스토그램 시각화

data(iris) # iris 데이터 셋 가져오기
names(iris) #"Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"  
str(iris) # 150   5
head(iris)
tail(iris)

summary(iris$Sepal.Length)

hist(iris$Sepal.Length, xlab="iris$Sepal.Length",
     col="magenta",
     main="iris 꽃받침 길이 histogram", xlim=c(4.3, 7.9))

summary(iris$Sepal.Width)

hist(iris$Sepal.Width, xlab="iris$Sepal.Width",
     col="mistyrose",
     main="iris 꽃받침 넓이 histogram", xlim=c(2.0, 4.5))

par(mfrow=c(1,2))

hist(iris$Sepal.Width, xlab="iris$Sepal.Width",
     col="green",
     main="iris 꽃받침 넓이 histogram", xlim=c(2.0, 4.5))

# 확률 밀도로 히스토그램 그리기 - 연속형변수의 확률 
hist(iris$Sepal.Width, xlab="iris$Sepal.Width",
     col="mistyrose",freq = F,
     main="iris 꽃받침 넓이 histogram", xlim=c(2.0, 4.5))

# 밀도를 기준으로 line을 그려준다.
lines(density(iris$Sepal.Width), col="red")

# 정규분포 곡선 추정 
par(mfrow=c(1,1))

hist(iris$Sepal.Width, xlab="iris$Sepal.Width",
     col="mistyrose",freq = F,
     main="iris 꽃받침 넓이 histogram", xlim=c(2.0, 4.5))

#밀도 분포곡선 추가
lines(density(iris$Sepal.Width), col="red")
#정규분포곡선 추가
x <- seq(2.0,4.5,0.1)
curve(dnorm(x, mean=mean(iris$Sepal.Width), sd=sd(iris$Sepal.Width)), 
      col="blue", add = T)

#3) 산점도 시각화
price<- runif(10, min=1, max=100) # 1~100사이 10개 난수 발생
price
plot(price)

par(mfrow=c(2,2)) # 2행 2열 차트 그리기
plot(price, type="l") # 유형 : 실선
plot(price, type="o") # 유형 : 원형과 실선
plot(price, type="h") # 직선
plot(price, type="s") # 꺾은선

# plot() 함수 속성 : pch : 연결점 문자타입-> plotting characher-번호(1~30)
plot(price, type="o", pch=5) # 빈 사각형
plot(price, type="o", pch=15)# 채워진 마름모
plot(price, type="o", pch=20, col="blue") #color 지정
plot(price, type="o", pch=20, col="orange", cex=1.5) #character expension(확대)

plot(price, type="o", pch=20, col="green", cex=2.0, lwd=3) #lwd : line width


methods(plot)

data()
WWWusage
str(WWWusage) # Time-Series
plot(WWWusage)

library(psych) # 패키지 로드
data(galton)
head(galton)

model <- lm(child ~ parent,data=galton)
plot(model)
#4) 동일데이터가 겹친 경우 시각화 표현

#[실습] 중복 데이터 시각화

# 1. vector 생성
x <- c(1,2,3,4,2,4)
y <- rep(2,6)
x
y

# 2. 교차테이블
table(x,y)
#   y
# x 2
# 1 1 <-1,2 (1)
# 2 2 <-2,2 (2)
# 3 1 <-3,2 (1)
# 4 2 <-4,2 (2)

# 3. 산점도
plot(x,y) #plot(y~x)

# 4. data.frame 형변환
t <- table(x,y)  #교차테이블
df <- as.data.frame(t) #data.frame 형변환
df #x y Freq(빈도수)

plot(x,y,pch=15,cex=0.8*df$Freq,col='blue')

par(mfrow=c(1,1)) 
library(psych) # 패키지 로드
data(galton) # galton 데이터 셋 가져오기 

# (1) 데이터프레임으로 변환 : 컬럼 단위의 데이터 활용을 위해서
# as.data.frame(table) : table 객체를 data.frame 객체로 변환

galtonData <- as.data.frame(table(galton$child, galton$parent))
head(galtonData) # Var1 Var2 Freq(중복 수) 
str(galtonData) # 154 obs(928 관측치가 중복 제외한 154개 관측치 생성 )
names(galtonData)=c("child","parent", "freq") # 컬럼에 이름 지정
head(galtonData)

# (2) 데이터프레임의 컬럼 단위로 추출 -> 숫자형으로 변환 
parent <- as.numeric(galtonData$parent)
child <- as.numeric(galtonData$child)

# (3) 점의 크기 확대 : 빈도수를 가중치로 적용 
plot(parent, child, pch=21, col="blue", bg="green",
     cex=0.2*galtonData$freq, xlab="parent", ylab="child")

#plot(jitter(parent), jitter(child), pch=21, col="blue", bg="green", xlab="parent", ylab="child")


#5) 변수 간의 비교 시각화
data(iris)
iris
attributes(iris)

# 4개 변수 상호비교
help(pairs)
pairs(iris[,1:4]) # Sepal.Length Sepal.Width Petal.Length Petal.Width

# Species=="virginica"인 경우만 4개 변수 상호비교
iris[iris$Species=="virginica", 1:4]

pairs(iris[iris$Species=="virginica", 1:4])
pairs(iris[iris$Species=="setosa", 1:4])

# 6. 차트 파일 저장 
setwd("C:/NCS/Rwork/output") # 폴더 지정
jpeg("iris.jpg", width=720, height=480) # 픽셀 지정 가능
plot(iris$Sepal.Length, iris$Petal.Length, col=iris$Species)
title(main="iris 데이터 테이블 산포도 차트")
dev.off() # 장치 종료 

### 3차원 산점도 
#########################
install.packages('scatterplot3d')
library(scatterplot3d)
# Fector의 levels 보기 
levels(iris$Species) # [1] "setosa"     "versicolor" "virginica" 

# 꽃의 종류별 분류 
iris_setosa = iris[iris$Species == 'setosa',]
iris_versicolor = iris[iris$Species == 'versicolor',]
iris_virginica = iris[iris$Species == 'virginica',]

# scatterplot3d(밑변, 오른쪽변, 왼쪽변, type='n') # type='n' : 기본 산점도 제외 
d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type='n')

d3$points3d(iris_setosa$Petal.Length, iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width, bg='orange', pch=21)

d3$points3d(iris_versicolor$Petal.Length, iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width, bg='blue', pch=23)

d3$points3d(iris_virginica$Petal.Length, iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width, bg='green', pch=25)



