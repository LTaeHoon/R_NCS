#################################
## <제3장 연습문제>
#################################   
# 01. 본문에서 작성한 info.df 변수를 다음과 같은 단계를 통해서
# infoprocess.csv 파일로 저장한 후 데이터프레임으로 가져오시오.
getwd()
setwd("c:/NCS/Rwork/output")
info.df <- read.csv("info.csv", header = T)
# 조건1)> "C:/Rwork/output" 디렉토리에 "infoprocess.csv"로 저장
# 힌트) write.csv()함수 이용
write.csv(info.df,"infoprocess.csv",row.names = FALSE)
# 조건2) "infoprocess.csv" 파일을 infoData 데이터 프레임으로 가져와서 결과 확인
infoData <- read.csv("infoprocess.csv",header = T, stringsAsFactors = F)
class(infoData)
# 조건3)  infoData 데이트 셋 구조 보기 함수를 이용하여 관측치와 컬럼수 확인
str(infoData)
# 조건4) 1980년과 1990년을 제외한 나머지 컬럼 대상으로 상위 6개 관측치 보기
head(infoData[,c(-2,-3)],6)

# 02. IMF 웹 사이트에 접속하여 세계금융 안정성 보고서에서 제공한 
# 주가 수익 비율 데이터(boxfigure1_1_1.csv)를 읽어 와서 다음과 
# 같은 결과가 출력되도록 데이터프레임을 생성하시오.

#조건) 사이트 주소 
#http://www.imf.org/external/pubs/ft/gfsr/2015/02/c1/boxfigure1_1_1.csv

imf_df<-read.csv("http://www.imf.org/external/pubs/ft/gfsr/2015/02/c1/boxfigure1_1_1.csv",header = T,stringsAsFactors = F)
imf_df <- imf_df[-c(1:7),-c(9:49)]
imf_df[1,1] <- "data"
names(imf_df)<-imf_df[1,]
imf_df <- imf_df[-1,]
head(imf_df) # 완성된 데이터 셋 보기
#      data   <0  0-10 11-20 20-50 50-100 100-200 >200
#9  1/1/2013  8.09  3.5  18.25  42.93    13.5     6.75    6.99
#10 1/2/2013  8.09  3.5  18.25  42.93    13.5     6.75    6.99
#11 1/3/2013  8.09  3.5  18.25  42.93    13.5     6.75    6.99
#12 1/4/2013  8.01 3.25  18.82  42.85   13.41     6.75    6.91
#13 1/7/2013  8.01 3.25  18.54   42.8    13.5     6.83    7.07
#14 1/8/2013  8.01 3.09  18.09  43.17   13.58     6.87     7.2