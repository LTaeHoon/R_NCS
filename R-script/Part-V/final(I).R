##########################
### Part-I 총정리 
##########################

# file.choose()함수를 이용하여 election_2012.csv 파일을 election 변수로 읽어온다.
election <- read.csv(file.choose()) # 2012년 미국 대선 후원금 현황 데이터 셋 
str(election)
# 데이터 셋 설명 : 2012년 미국 대선자('Romney, Mitt'와 'Obama, Barack') 후원금 현황
# 'data.frame':	1001731 obs. of  16 variables:
# 2. cand_id : 대선 후보자 id
# 3. cand_nm : 대선 후보자 이름
# 4. contbr_nm : 후원자 이름 
# 9. contbr_occupation : 후원자 직업군 
# 10. contb_receipt_amt: 후원금 


## chapter 02 : 필요한 칼럼 추출 ## 

# 문제 1) 위 5개 칼럼을 대상으로 election_df 이름으로 데이터 프레임을 생성하시오.
# (1) 칼럼 수를 이용하는 방법 
election_df <- election[c(2:4,9,10)]
str(election_df) # 'data.frame':	1001731 obs. of  5 variables:

# (2) 칼럼명을 이용하는 방법
election_df <- election[c('cand_id','cand_nm','contbr_nm','contbr_occupation','contb_receipt_amt')]
str(election_df) # 'data.frame':	1001731 obs. of  5 variables:


## chapter 02 : subset()함수 ##

# 문제 2) 'Romney, Mitt'와 'Obama, Barack' 대령통 후보자 별로 서브셋(subset)을 생성하시오.
# <조건1> romney와 obama 변수명으로 저장 
# <조건2> 각 후보자의 차원 보기 - dim() 함수 
# <조건3> 앞부분/뒷부분 6줄 관측치 확인 - head()/tail() 함수 
# 형식) 변수 <- subset(dataset, 조건식)
romney <- subset(election_df, election_df$cand_nm == 'Romney, Mitt')
obama <- subset(election_df, election_df$cand_nm == 'Obama, Barack')

dim(romney) # 107229      5
dim(obama) # 593746      5

head(romney); tail(romney)
head(obama); tail(obama)


# 문제 3) romney, obama 변수를 대상으로 후원금이 7000 달러 이상인 후원자들을 추출하여 
#   다음과 같이 처리하시오.
# <조건1> 추출된 결과 저장 변수 : romney_7000over, obama_7000over 
#       힌트) subset()함수 이용 
# <조건2> 각 후보자별로 후원자 수는 몇명인가 ?
# <조건3> 각 후보자별로 가장 많은 후원금의 기부자의 이름과 직업군은 ?

romney_7000over <- subset(romney, romney$contb_receipt_amt >= 7000)
obama_7000over <- subset(obama, obama$contb_receipt_amt >= 7000)
dim(romney_7000over) # 6  5
dim(obama_7000over) #  10  5
romney_7000over # NORPAC - 12,700
obama_7000over # OBAMA VICTORY FUND 2012 - UNITEMIZED, - 2,014,490.5


## chapter 03 : 파일 입출력 ## 

# 문제 4) romney, obama 변수를 대상으로 직업군이 공백인 관측치를 제거하여 서브셋을 생성하시오.
# <조건1> romney2, obama2 변수 저장 
# <조건2> 공백 제거 전과 후 관측치 차이 계산  
# <조건3> romney2와 obama2 변수를 romney.csv와 obama.csv 파일 저장(행번호 제외)
#    파일 저장 경로 : c:/Rwork/output

# 공백 제거 전 관측치 
dim(romney) # 107229      5
dim(obama) # 593746      5

# 직업군 공백없는 관측치 추출 
romney2 <- subset(romney, romney$contbr_occupation != '') # 직업군 공백 제외 
obama2 <- subset(obama, obama$contbr_occupation != '')
dim(romney2) # 106136      5
dim(obama2) # 589436      5

# 관측치 차이 
107229 - 106136 # 1093 - romney
593746 - 589436 # 4310 - obama 

# 파일 저장 
setwd('c:\\Rwork\\output')
write.csv(romney2, 'romney.csv', row.names = F) # 행번호 없이 저장 
write.csv(obama2, 'obama.csv', row.names = F)


## chap04 : 제어문 ##

# 문제 5) romney.csv, obama.csv 파일을 읽어와서 다음과 같이 처리하시오.
# <조건1> 저장할 변수명 : romney3, obama3
# <조건2> 후보자별 직업군이 'RETIRED'인 후원금만 추출하여 합계 계산
#    힌트) for()함수 이용 
# <조건3> 출력 결과 : OOO 후보자의 후원금 합계 : OOO 원 
#   힌트) cat()함수 이용 

romney3 <- read.csv('romney.csv', header = T)

romney3_retired_amt <- numeric() # 빈 vector 생성
idx = 1 # index 역할 
for(occ in romney3$contbr_occupation){ # 직업군을 vector로 사용 
   if(occ == 'RETIRED'){ # 직업군이 RETIRED인 경우 
     # 후원금을 빈 vector에 저장  
     romney3_retired_amt[idx] <- romney3$contb_receipt_amt[idx]
     idx <- idx + 1 # index 증가 
   }else{ #  RETIRED 아닌 경우 
     idx <- idx + 1 # index 증가
   }
}
# 결과 출력 
cat('romney 후보자의 RETIRED 후원금 합계 :',sum(romney3_retired_amt, na.rm=T), '원')
# romney 후보자의 RETIRED 후원금 합계 : 11266949 원

obama3 <- read.csv('obama.csv', header = T)

obama3_retired_amt <- numeric()
idx = 1 # index 역할 
for(occ in obama3$contbr_occupation){ # 직업군을 vector로 사용 
  if(occ == 'RETIRED'){ # 직업군이 RETIRED인 경우 
    # 후원금을 빈 vector에 저장  
    obama3_retired_amt[idx] <- obama3$contb_receipt_amt[idx]
    idx <- idx + 1 # index 증가 
  }else{ #  RETIRED 아닌 경우 
    idx <- idx + 1 # index 증가
  }
}
# 결과 출력 
cat('obama 후보자의 RETIRED 후원금 합계 :',sum(obama3_retired_amt, na.rm=T), '원')
# obama 후보자의 RETIRED 후원금 합계 : 25270507 원

25270507 - 11266949 # 14003558(obama 후보자가 더 후원금이 많음)


## chap04 : 내장함수(sort함수) ##

# 문제 6) romney3, obama3 변수를 대상으로 각 후보자별 가장 많은 후원자의 직업군 3개씩 확인하시오. 
#  힌트) table()함수, sort()함수 이용 
sort(table(romney3$contbr_occupation)) # 빈도수 -> 오름차순 
head(sort(table(obama3$contbr_occupation), decreasing = T),3) # 빈도수 -> 내림차순

head(sort(table(romney3$contbr_occupation), decreasing = T),3) # 빈도수 -> 내림차순


