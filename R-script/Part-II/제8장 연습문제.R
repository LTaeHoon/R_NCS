#################################
## <제8장 연습문제>
################################# 

#01. 다음 조건에 맞게 quakes 데이터 셋의 수심(depth)과 리히터규모(mag)가 
#동일한 패널에 지진의 발생지를 산점도로 시각화 하시오.
#조건1) 수심 3개 영역으로 범주화
#조건2) 리히터규모 2개 영역으로 범주화
#조건3) 수심과 리히터규모가 3행 2열 구조의 패널로 산점도 그래프 그리기
#힌트)  lattice 패키지의 equal.count()와 xyplot() 함수 이용
data(quakes)
str(quakes)
range(quakes$depth)
depthgroup <-equal.count(quakes$depth,number=3,overlap=0)
maggroup <- equal.count(quakes$mag,number=2,overlap=0)

xyplot(lat~long|maggroup*depthgroup,data=quakes,main="Fiji Earthquakes",
       ylab="latitude", xlab="longitude",pch=1,col=c("red","blue"))

# 02. latticeExtra패키지에서 제공되는 SeatacWeather 데이터 셋에서 월 별로 
# 최저기온과 최고기온을 선 그래프로 플로팅 하시오.
#힌트) lattice 패키지의 xyplot() 함수 이용
#힌트) 선 그래프 : type="l"

# 03. diamonds 데이터 셋을 대상으로 x축에 carat변수, y축에 price변수를 지정하고,
# clarity변수를 선 색으로 지정하여 미적 요소 맵핑 객체를 생성한 후 산점도 그래프 
# 주변에 부드러운 곡선이 추가되도록 레이아웃을 추가하시오.

# 04. roadmap 지도 유형으로 서울지역 주요 대학교에 Maker를 표시하시오.
#조건1) get_googlemap() 이용
#조건2) 지도 이미지 저장 : C:/Rwork/output/university.png

# 데이터 셋 가져오기 
library(ggmap)
university <- read.csv("C:/NCS/Rwork/Part-II/university.csv",header=T)
university # 구청명  LAT(경도)    LON(위도)      
uni <- round(data.frame(university$LON,university$LAT),digits = 2)
class(uni)
map <- get_googlemap('seoul', zoom=11, markers = uni, scale = 2)
ggmap(map, extent = 'device')
?get_googlemap

# 05. 각 지역별 총인구수를  'roadmap' 타입으로 시각화 하시오.
#조건1) 지도 중심 지역 Jeonju, zoom=7
#조건2) 지역명으로 텍스트 표시

# 데이터 셋 가져오기  
library(stringr)
pop <- read.csv("C:/NCS/Rwork/Part-II/population201506.csv",header=T)
kor<-get_map(location ="Jeonju", zoom=7, maptype='roadmap', scale=2)
pop$house <- as.numeric(str_replace_all(pop$세대수,',',''))

ggmap(kor)+geom_point(data=pop, aes(x=LON, y=LAT,color=house,size=house))
kor.map <- ggmap(kor)+geom_point(data=pop, aes(x=LON, y=LAT,color=factor(지역명)),size=3)


kor.map + geom_text(data= pop, aes(x=LON+0.01, y=LAT+0.01,label=지역명),size=5)
