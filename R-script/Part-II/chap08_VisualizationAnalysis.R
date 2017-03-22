#chap08_VisualizationAnalysis

#####################################
## Chapter08. 고급시각화 분석 
#####################################

# - lattice, latticeExtra, ggplot2, ggmap 패키지
##########################################
# 1. lattice 패키지
#########################################
# The Lattice Plotting System 
# 격자 형태의 그래픽(Trellis graphic) 생성 패키지
# 다차원 데이터를 사용할 경우, 한 번에 여러개의 plot 생성 가능
# 높은 밀도의 plot를 효과적으로 그려준다.
# lattice 패키지의 주요 함수
# xyplot(), barchart(), dotplot(),  stripplot(), cloud(), 
# histogram(), densityplot(), equal.count()
###########################################
available.packages()
install.packages("lattice")
library(lattice)
install.packages("mlmRev")
library(mlmRev)
data(Chem97)
###### Chem97 데이터 셋 설명 ##########
# - mlmRev 패키지에서 제공
# - 1997년 영국 2,280개 학교 31,022명을 대상으로 
#    A레벨(대학시험) 화학점수
# 'data.frame':  31022 obs. of  8 variables:
# score 변수 : A레벨 화학점수(0,2,4,6,8,10)
# gender 변수 : 성별
# gcsescore 변수 : 고등학교 재학중에 치루는 큰 시험(GCSE : 중등교수학능력정 인증시험)
# GCSE : General Certificate of Secondary Education)

str(Chem97) # data.frame':  31022 obs. of  8 variables:
table(Chem97$score)
head(Chem97,30) # 앞쪽 30개 레코드 


# 1.histogram(~x축, dataframe)
histogram(~gcsescore, data=Chem97) 
# gcsescore변수를 대상으로 백분율 적용 히스토그램
help()
histogram # histogram(~x축 | 조건, dataframe)
table(Chem97$score) #  0  2  4   6  8  10 <- 빈도수
# score 변수를 조건으로 지정 
histogram(~gcsescore | score, data=Chem97) # score 단위 
histogram(~gcsescore | factor(score), data=Chem97) # score 요인 단위

################################################
# 요인(factor) 수준은 그래픽 출력에 영향을 미친다.
# score를 x축에 대입할 경우 score가 출력되지만,
# factor로 변환된 score를 x축에 대입하면, 요인수준이 
# 순서로 적용된다.(0,2,4,6,8,10)
################################################

# 2.densityplot(~x축 | 조건, dataframe, groups=변수)

densityplot(~gcsescore | factor(score), data=Chem97, 
            groups = gender, plot.points=T, auto.key = T) 
# 밀도 점 : plot.points=F
# 범례: auto.key=T
# 성별 단위(그룹화)로 GCSE점수를 밀도로 플로팅    


# 차트 작성을 위한 데이터 리모델링 
# matrix -> data.freame형식으로 변경
# matrix -> data.table 형식으로 변경

# 1) 데이터셋 가져오기
data(VADeaths)
###### VADeaths 데이터셋 설명 #############
# 1941년 미국 버지니아주의 하위계층 사망비율 
# Rural Male(시골출신 남자), Urban Male(도시출신 남자))
###########################################
VADeaths
str(VADeaths)

# 2) 데이터셋 구조보기
mode(VADeaths) # numeric
class(VADeaths) # matrix

# 3) 데이터 리모델링
# (1) matrix -> data.frame 변환
df <- as.data.frame(VADeaths)
str(df) # 'data.frame':	5 obs. of  4 variables:
class(df) # "data.frame"
df 

# (2) matrix -> data.table 변환
dft <- as.data.frame.table(VADeaths)
str(dft) # 'data.frame':  20 obs. of  3 variables:
class(dft) # "data.frame"
dft # Var1  Var2 Freq -> 1열 기준으로 data.table 생성

# 3.barchart(y~x | 조건, dataframe, layout)
barchart(Var1 ~ Freq | Var2, data=dft, layout=c(4,1))
# Var2변수 단위(그룹화)로 x축-Freq, y축-Var1으로 막대차트 플로팅

# [실습] 0부터 시작
barchart(Var1 ~ Freq | Var2, data=dft, layout=c(4,1), origin=0)


# 4.dotplot(y~x | 조건 , dataframe, layout)
dotplot(Var1 ~ Freq | Var2 , dft) 

# [실습] 1행 4열 <- 결과 동일함
dotplot(Var1 ~ Freq | Var2 , dft, layout=c(4,1)) 

# Var2변수 단위로 그룹화하여 점을 연결하여 플로팅  
dotplot(Var1 ~ Freq, data=dft, groups=Var2, type="o", 
        auto.key=list(space="right", points=T, lines=T)) 
# type="o" : 점 타입 -> 원형에 실선 통과 
# auto.key=list(배치위치, 점 추가, 선 추가) : 범례 


# 5.xyplot(y축~x축| 조건, dataframe or list)
library(datasets)
str(airquality) # datasets의 airqulity 테이터셋 로드

###### airquality 데이터셋 설명 #############
# datasets 패키지에서 제공
# 대기오염에 관한 데이터셋
# 'data.frame':  153 obs. of  6 variables:
airquality

table(airquality$MONTH)

# Ozone Solar.R Wind Temp Month(5~9) Day

# airquality의 Ozone(y),Wind(x) 산점도 플로팅
xyplot(OZONE ~ WIND, data=airquality) 
range(airquality$OZONE,na.rm=T)
# Month(5~9)변수 기준으로 플로팅
xyplot(OZONE ~ WIND | factor(MONTH), data=airquality) # 2행3컬럼 
# default -> layout=c(3,2)
xyplot(OZONE ~ WIND | factor(MONTH), data=airquality, layout=c(5,1))
# 5컬럼으로 플로팅 - 컬럼 제목 : Month

# [실습] airquality 데이터셋의 Month 타입변경(factor)
convert <- transform(airquality, MONTH=factor(MONTH))
str(convert) # Month 변수의 Factor값 확인
# $ Month  : Factor w/ 5 levels "5","6","7","8"

convert # Ozone Solar.R Wind Temp Month Day
xyplot(Ozone ~ Wind | Month, data=convert, layout=c(5,1))
# 컬럼 제목 : Month 값으로 출력

#######################################
# factor로 변환된 Month를 x축에 대입하면, 요인수준이 
# 순서로 적용된다.(5,6,7,8,9)
#######################################
head(quakes)
str(quakes) # 'data.frame':  1000 obs. of  5 variables:
# lat, long, depth, mag, stations
range(quakes$stations)
############## quakes 데이터셋 설명 #################
# R에서 제공하는 기존 데이터셋
# - 1964년 이후 피지(태평양) 근처에 발생한 지진 사건 
#lat:위도,long:경도,depth:깊이(km),mag:리히터규모,stations  
####################################################

# 지진발생 위치(위도와 경로) 
xyplot(lat~long, data=quakes, pch=".") 
# 그래프를 변수에 저장
tplot<-xyplot(lat~long, data=quakes, pch=".")
# 그래프에 제목 추가
tplot2<-update(tplot,
               main="1964년 이후 태평양에서 발생한 지진위치")
print(tplot2)
tplot2
# depth 이산형 변수 리코딩
# 1. depth변수 범위
range(quakes$depth)# depth 범위
# 40 680
# 2. depth변수 리코딩
quakes$depth2[quakes$depth >=40 & quakes$depth <=150] <- 1
quakes$depth2[quakes$depth >=151 & quakes$depth <=250] <- 2
quakes$depth2[quakes$depth >=251 & quakes$depth <=350] <- 3
quakes$depth2[quakes$depth >=351 & quakes$depth <=450] <- 4
quakes$depth2[quakes$depth >=451 & quakes$depth <=550] <- 5
quakes$depth2[quakes$depth >=551 & quakes$depth <=680] <- 6

# 리코딩된 수심(depth2)변수을 조건으로 산점도 그래프 그리기
convert <- transform(quakes, depth2=factor(depth2))
xyplot(lat~long | depth2, data=convert)


# 동일한 패널에 2개의 y축에 값을 표현
# xyplot(y1+y2 ~ x | 조건, data, type, layout)

xyplot(OZONE + SOLAR.R ~ WIND | factor(MONTH), data=airquality,
       col=c("blue","red"),layout=c(5,1))

# 6. equal.count() : 지정된 범위 대상 영역구분과 카운팅

# [실습] 1~150 데이터를 대상으로 겹치지 않게 4개 영역으로 구분
numgroup<- equal.count(1:150, number=4, overlap=0)
numgroup
# Intervals:
#   min   max count
# 1   0.5  38.5    38
# 2  37.5  75.5    38
# 3  75.5 112.5    37
# 4 113.5 150.5    37

# (1) 지진의 깊이를 5영역으로 구분하여 카운팅
depthgroup<-equal.count(quakes$depth, number=5, overlap=0)
depthgroup
#Intervals:
#    min   max count
#1  39.5  80.5   203
#2  79.5 186.5   203
#3 185.5 397.5   203
#4 396.5 562.5   202
#5 562.5 680.5   200

# (2) depthgroup변수 기준으로 플로팅
xyplot(lat ~ long | depthgroup, data=quakes,
       main="Fiji Earthquakes(depthgruop)",
       ylab="latitude", xlab="longitude", pch=3,col='red' )

# (3) 리히터규모를 2개 영역으로 구분
magnitudegroup<-equal.count(quakes$mag, number=2, overlap=0)
magnitudegroup
#Intervals:
#   min  max count
#1 3.95 4.65   585
#2 4.55 6.45   516
# (4) magnitudegroup변수 기준으로 플로팅
xyplot(lat ~ long | magnitudegroup, data=quakes,
       main="Fiji Earthquakes(magjitude)",
       ylab="latitude", xlab="longitude", pch="@", col='blue')


# [실습] 수심과 리히터규모를 동시에 표현(2행 5열)
xyplot(lat ~ long | factor(depthgroup)*factor(magnitudegroup), data=quakes,
       main="Fiji Earthquakes",
       ylab="latitude", xlab="longitude",
       pch="@",col=c("red","blue"))
# 1행1열 : 수심(39.5 139.5   335), 리히터규모(3.95 4.65   585) 
# 2행1열 : 수심(39.5 139.5   335), 리히터규모(4.55 6.45   516) 

#-------------------------------------
# depth변수 리코딩
quakes$depth3[quakes$depth >=39.5 & quakes$depth <= 80.5] <- 'd1'
quakes$depth3[quakes$depth >=79.5 & quakes$depth <=186.5] <- 'd2'
quakes$depth3[quakes$depth >=185.5 & quakes$depth <=397.5] <- 'd3'
quakes$depth3[quakes$depth >=396.5 & quakes$depth <=562.5] <- 'd4'
quakes$depth3[quakes$depth >=562.5 & quakes$depth <=680.5] <- 'd5'

# mag변수 리코딩
quakes$mag3[quakes$mag >=3.95 & quakes$mag <= 4.65] <- 'm1'
quakes$mag3[quakes$mag >=4.55 & quakes$mag <= 6.45] <- 'm2'
# factor형 변환
convert <- transform(quakes, depth3=factor(depth3), mag3=factor(mag3))

# 산점도 그래프 그리기
xyplot(lat ~ long | depth3*mag3, data=convert,
       main="Fiji Earthquakes",
       ylab="latitude", xlab="longitude",
       pch="@",col=c("red","blue"))

# 7.coplot()
# a조건 하에서 x에 대한 y 그래프를 그린다.
# 형식) coplot(y ~ x : a, data)
# two variantes of the conditioning plot
# http://dic1224.blog.me/80209537545

# 기본 coplot(y~x | a, data, overlap=0.5, number=6, row=2)
# number : 조건의 사이 간격, 
# overlap : 겹치는 구간(0.1~0.9:작을 수록  사이 간격이 적게 겹침) 
# row : 패널 행수
coplot(lat~long | depth, data=quakes) # 2행3열, 0.5, 사이간격 6
coplot(lat~long | depth, data=quakes, overlap=0.1) # 겹치는 구간 : 0.1
coplot(lat~long | depth, data=quakes, number=5, row=1) # 사이간격 5, 1행 5열
coplot(lat~long | depth, data=quakes, number=5, row=1, panel=panel.smooth)
coplot(lat~long | depth, data=quakes, number=5, row=1, 
       col='blue',bar.bg=c(num='green')) # 패널과 조건 막대 색 

# 8.cloud()

# 3차원(위도, 경도, 깊이) 산점도 그래프 플로팅
cloud(depth ~ lat * long , data=quakes,
      zlim=rev(range(quakes$depth)), 
      xlab="경도", ylab="위도", zlab="깊이")

# 테두리 사이즈와 회전 속성을 추가하여 3차원 산점도 그래프 그리기
cloud(depth ~ lat * long , data=quakes,
      zlim=rev(range(quakes$depth)), 
      panel.aspect=0.9,
      screen=list(z=45,x=-25),
      xlab="경도", ylab="위도", zlab="깊이")

# depth ~ lat * long : depth(z축), lat(y축) * long(x축)
# zlim=rev(range(quakes$depth)) : z축값 범위 지정
# panel.aspect=0.9 : 테두리 사이즈
# screen=list(z=105,x=-70) : z,x축 회전
# xlab="Longitude", ylab="Latitude", zlab="Depth" : xyz축 이름


##########################################
# 2. latticeExtra
##########################################
# Extra Graphical Utilities Based on Lattice
# lattice 기반의 유용한 그래픽 함수를 추가한 패키지 
#필요한 패키지를 로딩중입니다: RColorBrewer
#필요한 패키지를 로딩중입니다: lattice
###########################################
install.packages("latticeExtra")
library(latticeExtra)
data(SeatacWeather) # latticeExtra 데이터
str(SeatacWeather) # 'data.frame':	90 obs. of  14 variables:


# 주요 함수 
# 1. doubleYScale() # latticeExtra
# (1) y축:강수량(precip) ~ x축:일(day) | 조건:월(month) , 직선 그래프(type="h")
rain <- xyplot(precip ~ day | month, data=SeatacWeather, type="h")
print(rain) # 월별 강수량 직선 그래프

# (2) y축(최저기온+최고기온)~x축:일(day) | 조건월(month), 선그래프(type="l")
temp <- xyplot(min.temp + max.temp ~ day | month,
               data=SeatacWeather, type="l", layout=c(3,1)) # type=line
print(temp)

# (3) 기온(2)과 강수량(1) 그래프 통합-doubleYScale(latticeExtra 제공)
doubleYScale(temp, rain,  style1=0, style2=3, add.ylab2=T, 
             text=c("min", "max", "rain"), columns=3)

# 왼쪽 y축 style(style1=0), 오른쪽 y축 style(style2=3), 
# layout : temp 기준
# 왼쪽 y축이름(min.temp + max.temp)
# add.ylab2=T : 오른쪽 Y축이름(precip), text : 범례 추가

# 직전 그래프에 색상 추가 
update(trellis.last.object(), 
       par.settings=simpleTheme(col=c("green","red","blue")))


# 2. ecdfplot() # latticeExtra
# New York Choral Society 합창단 성악가의 목소리 영역과 키 관계
# Heights of New York Choral Society singers 

data(singer, package = "lattice") 
head(singer)
str(singer) # 'data.frame': 235 obs. of  2 variables
names(singer)
# height voice.part
range(singer$height) # [1] 60 76
attributes(singer$voice.part) #변수의 속성보기


# Empirical CDF 누적분포 결과 예시 - Bass2 패널 
# 결과 : 전체 관측치 26에서 72가 7회 빈도수를 보여 
# Empirical CDF 26.9% 누적분포가 나타남

# Bass 2 서브셋 작성
data <- subset(singer, singer$voice.part=='Bass 2')
data1 <- subset(singer, singer$voice.part=='Soprano 1')
str(data) # [1] 26  2 -> Bass 2 관측치 26개
range(data$height) # [1] 66 75 -> Bass 2 키 범위
table(data$height) # Bass 2 키 빈도수
table(data1$height)
#66 67 68 69 70 71 72 74 75 
# 1  2  2  1  4  1  7  4  4 
7/26 # [1] 0.2692308 -> 누적분포 : 26.9% -> 누적그래프에서 확인
# 누적그래프 : y축의 전체 분포를 1로 표현한 상태에서 x축의 빈도수를
# 계단모양으로 누적하여 그래프가 그려진다. 

# Trellis Displays of Empirical CDF
# 실증적인(실험에 의한) 누적분포함수(Cumulative Distribution Functions)의
# 조건을 격자구조로 나타낸다.
# 누적분포함수 : 확률 계산에 있어서 확률변수의 특정 값 또는 
# 특정구간의 확률값(뿐만 아니라 특정값 이하의 모든 실수값)에 
# 대한 확률을 계산하는데 경우 이용하는 함수

ecdfplot(~height | voice.part, data = singer)
# factor형 변수(voice.part)를 조건으로 height변수에 CDF 적용 누적분포함수 


# 3. horizonplot() # latticeExtra
# EuStockMarkets : datasets 패키지제공 , Daily Closing Prices of Major European Stock Indices, 1991-1998
# Daily Closing Prices of Major European Stock Indices, 1991–1998
# 1991~1998년 유럽의 주요 주가 지수의 일일 마감 가격
# 년도에 의해서 4개의 칼럼(DAX(독일),SMI(스위스),CAC(프랑스),FTSE(영국))이 구성되며, 
# 각 컬럼 당 1860개의 데이터로 구성되어 있다.
# 년도별도 4개의 주식에 대한 주가 지수가 시계열 데이터로 표현   
head(EuStockMarkets) # DAX    SMI    CAC   FTSE
str(EuStockMarkets) # mts [1:1860, 1:4] 1629 1614 1607 1621 1618 ...
attributes(EuStockMarkets)
#$class
#[1] "mts"    "ts"     "matrix"
summary(EuStockMarkets) #주식별 요약통계량

EuStockMarkets
# Plot many time series in parallel by cutting the y range into segments and 
# overplotting them with color representing the magnitude and 
# direction of deviation.

# 시계열 자료를 병렬로 플로팅해주수는 함수  

horizonplot(EuStockMarkets, colorkey = TRUE, 
            origin = 4000, horizonscale = 1000)  
# colorkey = TRUE : 범례
# horizonscale = 1000 : 가로 범위(1860에서 1000까지만)
# origin = 4000 : 중심위치


# 4. mapplot() # latticeExtra

# This data set records the annual rates of death (1999-2003) due to cancer by sex in US counties. 
# 1999~ 2003년도에 미국 도시에서 sex에 의한 암의 원인으로 사망한 비율
head(USCancerRates) #  latticeExtra 패키지 제공
data(USCancerRates)
str(USCancerRates) # 'data.frame':	3041 obs. of  8 variables:

#Produces Trellis displays of numeric (and eventually categorical) data on a map.
#This is largely meant as a demonstration, and users looking for serious map 
#drawing capabilities should look elsewhere (see below).
# Draw lines and polygons as speciﬁed by a map database.

install.packages("maps")
install.packages("mapproj")
library(maps) #  map()함수
library(mapproj) # projection 지원


# maps 패키지에서 제공되는 map()함수 
# map database을 이용하여 특별한 선이나 다각형을 그려준다. 
# database와 투사방법에 의해서 지도가 그려진다.  
# map(database, plot, fill, projection)
map = map("county", plot = F, fill = T, projection = "mercator")
# database="world", usa(usa,state,county-주 경계)
# This database produces a map of the counties of the United States mainland generated from US Department of the Census data (see the reference).
# county : 미국인구조사국의 데이터에 의해서 미국 영토의 주 지도가 만들어진 데이터베이스 
# plot = T : 플로팅 여부
# fill = TRUE : 바인딩된 데이터에 의해서 색이 채워진다.
# projection=mercator, tetra, azequalarea : 투사 방법
# projection = "mercator" : 네덜란드의 지리학자-메르카토르의 투영법 : 지도 제작법 
# 투사 방법에 따라서 크기와 모양이 조금씩 달라진다.

# Produces Trellis displays of numeric (and eventually categorical) data on a map. 
# 지도의 수치 데이터를 격자 구조로 그려준다.
#This is largely meant as a demonstration, and users looking for serious map 
# 주로 사용자가 지도를 보있 있는 데모를  실제 실제 지도와 유사한 데모ㅇ를 
#drawing capabilities should look elsewhere (see below).

# mapplot(y축 ~ x축 , data, map)
# y축 : USCancerRates 행이름(주 이름)
# x축 : 남자 사망 비율, 여자 사망비율 
mapplot(rownames(USCancerRates) ~ log(rate.male) + log(rate.female), 
        data = USCancerRates, map)

#Warning messages: 제거
suppressWarnings(print(mapplot()))


###########################################
# 3. ggplot2 패키지
###########################################
# ggplot2 그래픽 패키지
# 기하학적 객체들(점,선,막대 등)에 미적 특성(색상, 모양,크기)를 
# 맵핑하여 플로팅한다.
# 그래픽 생성 기능과 통계변환을 포함할 수 있다.
# ggplot2의 기본 함수 qplot()
# geoms(점,선 등) 속성, aes(크기,모양,색상) 속성 사용
# dataframe 데이터셋 이용(변환 필요)
###########################################


install.packages("ggplot2") # 패키지 설치
library(ggplot2)

data(mpg) # 데이터 셋 가져오기
str(mpg) # map 데이터 셋 구조 보기
head(mpg) # map 데이터 셋 내용 보기 
summary(mpg) # 요약 통계량
table(mpg$drv) # 구동방식 빈도수 
################ mpg 데이터셋 #################
# ggplot2에서 제공하는 데이터셋
# 'data.frame':	234 obs. of  11 variables:
# 주요 변수 : displ:엔진크기, cyl : 실린더수,
#      drv(구동방식) ->사륜구동(4), 전륜구동(f), 후륜구동(r)
###################################################


# 1. qplot()함수
help(qplot)

# (1) 1개 변수 대상 기본 - 속이 꼭찬 막대 모양의 세로막대 그래프 
qplot(hwy, data=mpg) # 세로막대 그래프
#  fill 옵션 : hwy 변수를 대상으로 drv변수에 색 채우기(누적 막대그래프) 
qplot(hwy, data=mpg, fill=drv) # fill 옵션 적용
# binwidth 옵션 : 막대 폭 지정 옵션
qplot(hwy, data=mpg, fill=drv, binwidth=2) # binwidth 옵션 적용 
# facets 옵션 : drv변수 값으로 칼럼단위와 행 단위로 패널 생성
qplot(hwy, data=mpg, fill=drv, facets=.~ drv, binwidth=2) # 열 단위 패널 생성
qplot(hwy, data=mpg, fill=drv, facets=drv~., binwidth=2) # 행 단위 패널 생성


# (2) 2변수 대상 기본 - 속이 꽉찬 점 모양과 점의 크기는 1를 갖는 산점도 그래프
qplot(displ, hwy, data=mpg)# mpg 데이터셋의 displ과 hwy변수 이용
# displ, hwy 변수 대상으로 drv변수값으로 색상 적용 산점도 그래프
qplot(displ, hwy, data=mpg, color=drv)


# <실습> 엔진크기와 고속도로 주행속도와의 관계를 구동방식으로 구분
qplot(displ, hwy, data=mpg, color=drv, facets=.~ drv)
# mpg 데이터셋의 displ과 hwy변수 이용

# (3) 색상, 크기, 모양 적용
### ggplot2 패키지 제공 데이터 셋
head(mtcars)
str(mtcars) # ggplot2에서 제공하는 데이터 셋
qplot(wt, mpg, data=mtcars, color=factor(carb)) # 색상 적용
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb)) # 크기 적용
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb), shape=factor(cyl))#모양 적용 
mtcars$qsec


# (4) geom 옵션 
### ggplot2 패키지 제공 데이터 셋
head(diamonds) 

# geom="bar" -> clarity변수 대상 cut변수로 색 채우기
qplot(clarity, data=diamonds, fill=cut, geom="bar") # 레이아웃에 색 채우기
qplot(clarity, data=diamonds, colour=cut, geom="bar") # 테두리 색 적용


# geom="point"
# qplot(wt, mpg, data=mtcars, size=qsec) # geom="point" 기본
qplot(wt, mpg, data=mtcars, size=qsec, geom="point")
# cyl 변수의 요인으로 point 크기 적용, carb 변수의 요인으로 포인트 색 적용
qplot(wt, mpg, data=mtcars, size=factor(cyl), color=factor(carb), geom="point")
# qsec변수로 포인트 크기 적용, cyl 변수의 요인으로 point 모양 적용
qplot(wt, mpg, data=mtcars, size=qsec, color=factor(carb), shape=factor(cyl), geom="point")

# geom="smooth"
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"))
qplot(wt, mpg, data=mtcars, color=factor(cyl), geom=c("point", "smooth"))
# cyl변수 요인으로 색상 적용


# geom="line"
qplot(mpg, wt, data=mtcars, color=factor(cyl), geom="line")
#qplot(mpg, wt, data=mtcars, color=factor(cyl), geom="point") + geom_line()
# [실습] 결과 동일
#qplot(mpg, wt, data=mtcars, color=factor(cyl), geom=c("point","line"))

#  geom="freqpoly"
qplot(clarity, data=diamonds, geom="freqpoly", group=cut, colour=cut)

# (5) position 옵션 
# 다양한 bar 차트 유형("identity",stacked, dodged, identity)
head(diamonds)
# 채우기-가장 큰 값을 기준으로 채우기형 막대그래프
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="identity")
# 채우기-가장 적은 값을 기준으로 채우기형 막대그래프 
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="fill")
# 스택형태-누적형-기본형
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="stack")
# 다지-살짝 비키다
qplot(clarity, data=diamonds, geom="bar", fill=cut, position="dodge")


# 2. ggplot()함수

# (1) aes(x,y,color) 옵션 
# aes(x,y,color) 속성 = aesthetics : 미학
p <-ggplot(diamonds, aes(carat, price, color=cut))


#geom_point()함수 - 결과 동일 
p<-ggplot(diamonds, aes(carat, price, color=cut))
p+geom_point()  # point 추가


# (2) geom_line() 레이어 추가 
p<- ggplot(mtcars, aes(mpg,wt,color=factor(cyl)))
p+geom_line() # line 추가

# (3) geom_point()함수  레이어 추가
p<- ggplot(mtcars, aes(mpg,wt,color=factor(cyl)))
p+geom_point()  # point 추가

# (4) geom_step() 레이어 추가
p<- ggplot(mtcars, aes(mpg,wt,color=factor(cyl)))
p+geom_step()  # step 추가

# (5) geom_bar() 레이어 추가
p<- ggplot(diamonds, aes(clarity))
p+geom_bar(aes(fill=cut), position="dodge")  # bar 추가

p<- ggplot(diamonds, aes(clarity))
p+geom_bar(aes(fill=cut), position="fill")  # bar 추가

p<- ggplot(diamonds, aes(clarity))
p+geom_bar(aes(fill=cut), position="stack")  # bar 추가

## create a pie-chart(파이 차트), radar-chart(레이더 차트) (hint: not recommended)
# map a barchart to a polar coordinate system
p.tmp <- ggplot(mtcars, aes(x=factor(1), fill=factor(cyl))) + geom_bar(width=1)
p.tmp
p.tmp + coord_polar(theta="y")
p.tmp + coord_polar()
ggplot(mtcars, aes(factor(cyl), fill=factor(cyl))) + geom_bar(width=1) + coord_polar()
#########################################################

# (6) stat_bin() : ggplot() 결과 미적요소와 기하객체를 추가 역할
p <- ggplot(diamonds, aes(price)) 

p + stat_bin(aes(fill=cut), geom="bar")
p + stat_bin(aes(fill=..density..), geom="bar") # price 빈도를 밀도(전체의 합=1)로 스케일링 
p + stat_bin(aes(fill=cut), geom="area")
p + stat_bin(aes(color=cut, size=..density..),geom="point") 

# (7) Theme
#테마의 각각의 요소들은 특정한 상속 관계에 의해서 상속되어 진다.
p <- ggplot(diamonds, aes(carat, price, color=cut)) 
p <- p + geom_point() 
print(p) # 테마가 적용될 산점도 그래프 

p + theme_gray() # 기본 테마
p + theme_bw() # 흰색 
p + theme_bw(18) # 크게
p + theme_classic() # 고전
p + theme_minimal() # 고전 + 격자

# (8) 제목 추가, 외형 속성 설정  
p <- ggplot(diamonds, aes(carat, price, color=cut)) 
p <- p + geom_point() + ggtitle("다이아몬드 무게와 가격의 상관 관계") 
print(p) # 제목이 적용된 산점도 그래프 


# 그래프 외형 속성 설정
p + theme(
  title=element_text(color="blue", size=25),
  axis.title=element_text(size=14, face="bold"), #축 제목
  axis.title.x=element_text(color="green"), #x축 제목
  axis.title.y=element_text(color="green"), #y축 제목
  axis.text=element_text(size=14),   # 축이름 크기
  axis.text.y=element_text(color="red"),   # y축 이름 
  axis.text.x=element_text(color="purple"),  # x축 이름
  legend.title=element_text(size=20, face="bold",color="red"), #범례 
  legend.position="bottom",  # 범례 위치
  legend.direction="horizontal" # 방향
) 


# 3. ggsave()함수 
# save image of plot on disk 
#geom_point()함수 - 결과 동일 
p<-ggplot(diamonds, aes(carat, price, color=cut))
p+geom_point()  # point 추가
ggsave(file="C:/NCS/Rwork/output/diamond_price.pdf") # 가장 최근 그래프 저장
ggsave(file="C:/NCS/Rwork/output/diamond_price.jpg", dpi=72)

# 변수에 저장된 그래프 저장 
p<- ggplot(diamonds, aes(clarity))
p<- p+geom_bar(aes(fill=cut), position="fill")  # bar 추가
ggsave(file="C:/NCS/Rwork/output/bar.png", plot=p, width=10, height=5)


##########################################
# 4. ggmap 패키지
##########################################
#공간시각화
# 공간 시각화는 지도를 기반으로 하기 때문에 
# 위치, 영역, 시간과 공간에 따른 차이 및 변화에 대한것을 다룸
# 위치특징 : 위도 및 경도, 지도에 버블로 표현
# 표현방법 : 레이어 형태로 추가하여 시각화
# 영역 : 데이터에 따른 색상으로 표현
##########################################         

#geocode : 거리주소 또는 장소 이름을 이용하여 이용 지도 정보(위도, 경도) 획득 
#get_googlemap :  구글 지도 서비스(Google Static Maps) API에 접근하여 정적 지도 다운로드 
#get_map : 지도 서비스 관련 서버(GoogleMaps, OpenStreetMap, StamenMapsor, Naver Map)에 
# 관련된 질의어를 지능형으로 인식하여 지도 정보 생성
#get_navermap:네이버 지도 서비스(Naver Static Maps) API에 접근하여 정적 지도 다운로드 지원
#ggimage : ggplot2 이미지와 동등한 수준으로 지도 이미지 생성
# ggmap  : get_map()함수에 의해서 생성된 픽셀 객체를 지도 이미지로 시각화
# ggmapplot :get_map()함수에 의해서 생성된 픽셀 객체를 지도 이미지로 시각화
#qmap : ggmap()함수와 get_map() 함수의 통합 기능
#qmplot : ggplot2패키지의 qplot()와 동등한 수준으로 빠르게 지도 이미지 시각화

#---------ggmap()과 ggmapplot()함수 옵션 차이점
ggmap(ggmap, extent = "panel", base_layer, maprange = FALSE, 
      legend = "right", padding = 0.02, darken = c(0, "black"), ...)
ggmapplot(ggmap, fullpage = FALSE, base_layer, maprange = FALSE, 
          expand = FALSE, ...)

#----------------------------------------------

#[실습1] geocode(), get_googlemap()함수
# 지도 관련 패키지 설치
install.packages("ggplot2")
install.packages("stringr")
library(stringr)
library(ggplot2)
install.packages("ggmap") # ‘ggmap’와 ‘ggplot2’(우선 설치) 관련 패키지
library(ggmap)
#-----------------[그림 12-3]---------------
map <- get_map(location ="seoul", zoom=10, maptype='roadmap', source = "google")
ggmap(map, size=c(600,400), extent='device')
#-------------------------------------------
#  지도위치정보를 이용하여 지도정보를 생성하고, 지도 이미지 그리기  
#(e.g. "1600 pennsylvania avenue, washington dc" or "Baylor University") 
# (1) 지도위치정보 가져오기
gc <- geocode("seoul")
gc
#gc <- geocode("seoul, korea") # geolocation API 이용 
gc <- geocode("waco, texas") # texas주 중부 도시
class(gc)
mode(gc)
center <- as.numeric(gc)
center # 위도,경도
#[1] 126.97797  37.56654

#(2) 지도 정보 생성하기
map <- get_googlemap(center = center, language="ko-KR", color = "bw", scale = 2 )
# bw :  black-and-white - 흰색 바탕에 검은색 글자
# scale :  1, 2, or 4 (scale = 2 : 1280x1280 pixels) 

# (3) 지도 이미지 그리기
ggmap(map, extent = 'device')
# extent : 지도가 그려질 크기를 지정하는 옵션
#  ("normal", "device", or "panel" (default))

# [실습2] round(), get_googlemap()함수
# (1) markers 데이터프레임 생성 -> round 적용 
df <- round(data.frame( x = jitter(rep(-95.36, 25), amount = .3), 
                        y = jitter(rep(29.76, 25), amount = .3) ), digits = 2)
df

# (2) 지도 위에 markers 적용 
map <- get_googlemap('houston', markers = df, path=df, scale = 2)
ggmap(map, extent = 'device')

# [실습2] get_googlemap() vs get_map함수
# (1) get_googlemap() 지도 이미지 그리기
#get_googlemap(center, zoom, size, scale, format, maptype, language,color, markers, path)
map <- get_googlemap('seoul',  scale = 2)
ggmap(map, extent = 'device')

# (2) get_map() 지도 이미지 그리기-다양한 지도 유형 제공
#get_map(center, zoom, scale, maptype, source, color, language, api_key) 
map <- get_map('seoul',  scale = 2)
ggmap(map, extent = 'device')


#[실습3] get_map함수
map <- get_map(location ="london", zoom=14, maptype='roadmap', scale=2)
# get_map("중심지역", 확대비율, 지도유형) : ggmap에서 제공하는 함수 
ggmap(map, size=c(600,600), extent='device')

map <- get_map(location ="seoul", zoom=14, maptype='watercolor', scale=2)
ggmap(map, size=c(600,600), extent='device')

# zoom 차이
map <- get_map(location ="seoul", zoom=14, scale=2)
map <- get_map(location ="seoul", zoom=8, scale=2)
ggmap(map, size=c(600,600), extent='device')

# source 차이
map <- get_map(location = "texas", zoom = 6, source = "stamen")
# stamen : maptype='satellite' 지원 안됨
map <- get_map(location ="seoul", source = "osm", zoom=8, maptype='watercolor')
ggmap(map, size=c(600,600), extent='device')
map <- get_map(location = "seoul",zoom=14)
ggmap(map, size=c(600,600), extent='device')


# 실습 데이터-서울지역 4년제 대학교 위치 표시
university <- read.csv("C:/NCS/Rwork/Part-II/university.csv",header=T)
university # # 학교명","LAT","LON"

# (1)레이어1 : 정적 지도 생성
kor <- get_map("seoul", zoom=11, maptype = "watercolor")#roadmap
# maptype : roadmap, satellite, terrain, hybrid

# (2)레이어2 : 지도위에 포인트
ggmap(kor)+geom_point(data=university, aes(x=LON, y=LAT,color=factor(학교명)),size=3)
kor.map <- ggmap(kor)+geom_point(data=university, aes(x=LON, y=LAT,color=factor(학교명)),size=3)

# (3)레이어3 : 지도위에 텍스트 추가
kor.map + geom_text(data=university, aes(x=LON+0.01, y=LAT+0.01,label=학교명),size=5)
# LAT+0.01 : 텍스트 위치(포인트의 0.01 위쪽)
# geom_text : 텍스트 추가

# (4)지도 저장
# 넓이, 폭 적용 파일 저장
ggsave("C:/NCS/Rwork/output/university1.png",width=10.24,height=7.68)
# 밀도 적용 파일 저장
ggsave("C:/NCS/Rwork/output/university2.png",dpi=1000) # 9.21 x 7.68 in image

#<공간시각화 실습>

# 2015년도 06월 기준 대한민국 인구수
pop <- read.csv("C:/NCS/Rwork/Part-II/population201506.csv",header=T)
pop

region <- pop$지역명   
lon <- pop$LON # 위도
lat <- pop$LAT # 경도
house <- pop$세대수

# 위도,경도,세대수 이용 데이터프레임 생성
df <- data.frame(region,lon,lat,house)
df
# 지도정보 생성
map1 <- get_map("daegu", zoom=7 ,  maptype='watercolor')
#map1 <- get_map("daegu", zoom=7 ,  maptype='roadmap')

# 레이어1: 지도 플로팅
map2 <- ggmap(map1)
map2

# 레이어2 : 포인트 추가
map2 + geom_point(aes(x=lon,y=lat,colour=house,size=house),data=df)
map3 <- map2 + geom_point(aes(x=lon,y=lat,colour=house,size=house),data=df)

# 레이어3 : 텍스트 추가
map3 + geom_text(data=df, aes(x=lon+0.01, y=lat+0.18,label=region),size=3)

# 크기, 넓이, 폭 적용 파일 저장
ggsave("C:/NCS/Rwork/output/population201506.png",scale=1,width=10.24,height=7.68)

# [실습] 세종 자치시 추가
#---------------------------------------------------

# 다양한 지도 유형

# maptype='terrain'
map1 <- get_map("daegu", zoom=7 ,  maptype='terrain')
map2 <- ggmap(map1)
map3 <- map2 + geom_point(aes(x=lon,y=lat,colour=house,size=house),data=df)
map3 + geom_text(data=df, aes(x=lon+0.01, y=lat+0.18,label=region),size=3)


# maptype='satellite'
map1 <- get_map("daegu", zoom=7 ,  maptype='satellite')
map2 <- ggmap(map1)
map3 <- map2 + geom_point(aes(x=lon,y=lat,colour=house,size=house),data=df)
map3 + geom_text(data=df, aes(x=lon+0.01,y=lat+0.18,colour=region,label=region),size=3)

# maptype='roadmap'
map1 <- get_map("daegu", zoom=7 ,  maptype='roadmap')
map2 <- ggmap(map1)
map3 <- map2 + geom_point(aes(x=lon,y=lat,colour=house,size=house),data=df)
map3 + geom_text(data=df, aes(x=lon+0.01, y=lat+0.18,label=region),size=3)

# maptype='hybrid'
map1 <- get_map("jeonju", zoom=7,  maptype='hybrid')
map2 <- ggmap(map1)
map3 <- map2 + geom_point(aes(x=lon,y=lat,colour=house,size=house),data=df)
map3 + geom_text(data=df, aes(x=lon+0.01, y=lat+0.18,label=region),size=3)
map3 + geom_density2d()
# 밀도레이어 그래프 추가(geom_density2d)

# ---------------------------------------------------------------------
# stat_bin2d() : 버블모양 적용함수, 인구수(data) 기준 색 적용(많이 사용)
map1 <- get_map("jeonju", zoom=7 ,  maptype='hybrid')
map2 <- ggmap(map1)
map2 + stat_bin2d(aes(x=lon,y=lat,colour=house,fill=factor(house),size=house),data=df)





