# chap17_TimeseriesAnalysis






########################################################

        # 시계열분석

########################################################
# 시계열(Time-Series) : 관측치 또는 통계량의 변화를 시간의 움직임에 
# 따라서 기록하고, 이것을 계열화한 것을 의미한다.
# 시계열 데이터 : 통계숫자를 시간의 흐름에 따라 일정한 간격마다
# 기록한 통계열을 의미한다.

# 관련분야 : 경기예측, 판매예측, 주식시장분석, 예산분석, 투자연구 등
# 현상 이해 -> 미래 예측

############################
# 시계열 자료 확인
############################

#단계 1 : AirPassengers 데이터 셋 가져오기
    data(AirPassengers) # 12년간 항공기 탑승 승객 수 

    #R에서 제공되는 기본 데이터 셋으로 12년(1949~1960년) 동안 매월 항공기 탑승의 승객 수(시계열 자료)


#단계 2 : 차분 적용 : 평균 정상화  
    par(mfrow=c(1,2))
    ts.plot(AirPassengers) # 시계열 시각화
    diff <- diff(AirPassengers) # 차분 수행 
    plot(diff) # 평균 정상화 

    
#단계 3 : 로그 적용 : 분산 정상화  
    par(mfrow=c(1,2))
    plot(AirPassengers) # 시계열 시각화  
    log <- diff(log(AirPassengers)) # 로그+차분 수행 
    plot(log) # 분산 정상화 

    
    
############################
# 시계열 추세선 시각화
############################

    data(WWWusage) # WWWusage   Internet Usage per Minute
    
    str(WWWusage) # Time-Series [1:100] from 1 to 100:
    # format : A time series of length 100.
    # R에서 제공되는 기본 데이터 셋으로 인터넷 사용 시간을 분 단위로 
    # 측정한 100개 시계열 데이터이다. 
    WWWusage
    
    # 추세선 시각화
        par(mfrow=c(1,1))
        plot(WWWusage, type="l", col='red')
        # ts.plot(WWWusage, type="l", col='red')
    
    # 데이터프레임 생성 
        InternetUsage <- WWWusage # 인터넷 사용시간 벡터 생성(단위 : 분)
        Minute<- c(1:100) # 분 단위 벡터 
    
    # 데이터프레임 생성 
        Timeseries <- data.frame(Minute,InternetUsage)
        head(Timeseries)
    
    # 추세선 그리기 
        library(ggplot2)
        ggplot(Timeseries, aes(x=Minute, y=InternetUsage)) + geom_point(color="blue")
        ggplot(Timeseries, aes(x=Minute, y=InternetUsage))+ geom_line(color="red")

        
        
        
# [실습] 시계열 데이터 추세선 시각화 
        
    data(EuStockMarkets)
    head(EuStockMarkets)
    str(EuStockMarkets) # mts [1:1860, 1:4] 2차원 matrix 구조  
    
    EuStock<- data.frame(EuStockMarkets)
    EuStock
    head(EuStock)
    # 단일 시계열 데이터 추세선 
    plot(EuStock$DAX[1:1000], type="l", col='red') # 선 그래프 시각화 
    
    # 다중 시계열 데이터 추세선
    plot.ts(cbind(EuStock$DAX[1:1000],EuStock$SMI[1:1000]),main="주가지수 추세선")
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
###################################
    
# 시계열요소분해 시각화
    
###################################
    
    # 단계 1 : 시계열자료 준비 
        data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 
                  55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 
                  56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
        length(data)# 36
    
        
    #단계 2 : 시계열자료 생성 : 시계열자료 형식으로 객체 생성
        tsdata <- ts(data, start=c(2016, 1), frequency=12) 
        tsdata # 2016~2018
    
        par(mfrow=c(1,2))
        
    #단계 3 : 추세선 확인 
        #각 요인(추세, 순환, 계절, 불규칙)을 시각적으로 확인한다.
        ts.plot(tsdata) # plot(tsdata)와 동일함 
    
        
    #단계 4 : 시계열 분해
        plot(stl(tsdata, "periodic")) # periodic : 주기적인 
    
        
        
        
    #단계 5 : 시계열 분해와 변동 요인 제거 
        m <- decompose(tsdata) # decompose()함수 이용 시계열 분해
        attributes(m) # 변수 보기
        
        plot(m) # 추세요인, 계절요인, 불규칙요인이 포함된 그래프     
        plot(tsdata - m$seasonal)  # 계절요인을 제거한 그래프 
    
    #단계 6 : 추세요인과 불규칙요인 제거
        plot(tsdata - m$trend) # 추세요인 제거 그래프
        plot(tsdata - m$seasonal - m$trend) # 불규칙요인만 출력
    
    
    
########################################

    # 자기상관함수/부분자기상관함수 
    
########################################
    
    # 자기상관함수/부분자기상관함수 : 자기상관계수 확인
    # 자기상관계수는 서로 이웃한 시점 간의 상관계수
    # 시계열에서 Yt와 Yt-1 간의 상관계수 
    # 자기상관계수 분석 = 잔차의 독립성(DW > 0.05)
    
    #단계 1 : 시계열자료 생성 
    input <- c(3180,3000,3200,3100,3300,3200,3400,3550,3200,3400,3300,3700) 
    length(input) # 12
    tsdata <- ts(input, start=c(2015, 2), frequency=12) # Time Series
    
    par(mfrow=c(1,1))
    
    #단계 2 : 자기상관함수 시각화 
    acf(na.omit(tsdata), main="자기상관함수", col="red")
    # 그래프가 선(유의수준) 안에 들어와 있으면 자기상관이 없다.
    
    
    #단계 3 : 부분자기상관함수 시각화
    pacf(na.omit(tsdata), main="부분자기상관함수", col="red")
    
    
    
    
    
    
    
    
    
    
    
    
##################################
    
    # ARIMA모형 분석 절차 
    
##################################
    
#[단계1] 시계열자료 특성분석(정상성/비정상성)
#[단계2] 정상성시계열 변환
#[단계3] 모형 식별과 추정
#[단계4] 모형 생성 
#[단계5] 모형 진단(모형 타당성 검정)
#[단계6] 미래 예측(업무 적용)

    
## 1. 정상성시계열의 비계절형 ##
    
    # [단계1] 시계열자료 특성분석 
    
        #(1) 데이터 준비  
            input <- c(3180,3000,3200,3100,3300,3200,3400,3550,3200,3400,3300,3700) 
        
        #(2) 시계열객체 생성(12개월 : 2015년 2월 ~ 2016년 1개)
            tsdata <- ts(input, start=c(2015, 2), frequency=12) 
            tsdata  
            #<해설> stats 패키지에서 제공하는 ts()함수를 이용하여 벡터 자료(input)를 대상으로 시계열 객체(tsdata)를 생성한다.
        
        #(3) 추세선 시각화
            plot(tsdata, type="l", col='red')
            # 비정상성시계열로 판단되어 차분을 통해서 정상성시계열로 변경할 필요
        
        
    # [단계2] 정상성시계열 변환
        # 차분을 통해서 비정상성시계열을 정상성시계열로 변환한다. 
        # 차분은 일반차분과 계절차분으로 구분되는데, 계절성을 갖는 경우에는 계절차분 적용
    
        par(mfrow=c(1,1))
        ts.plot(tsdata, col="red")
        diff <- diff(tsdata)
        plot(diff) # 차분 : 현시점에서 이전시점의 자료를 빼는 연산
        
        
        
    # [단계3] 모형 식별과 추정
        # R를 이용한 시계열분석에서는 auto.arima() 함수를 이용한다. 
        # forecast 패키지에서 제공되는 auto.arima() 함수는 시계열 모형을 식별하는 
        # 알고리즘에 의해서 최적의 모형과 파라미터를 추정하여 제공   
        install.packages('forecast')
        library(forecast)
        arima <- auto.arima(tsdata) # 시계열 데이터 이용 
        arima
        #Series: tsdata 
        #ARIMA(1,1,0)    <- ARMA(1, 0) 모형으로 식별
    
    
    # [단계4] 모형 생성 
        #이전 단계에서 식별된 모형과 파라미터를 이용하여 시계열 모형을 생성한다.  
        model <- arima(tsdata, order=c(1, 1, 0))
        model 

                
    # [단계5] 모형 진단(모형 타당성 검정)
    
        #(1) 자기상관함수에 의한 모형 진단
            tsdiag(model)
        
        #(2) Box-Ljung에 의한 잔차항 모형 진단
            Box.test(model$residuals, lag=1, type = "Ljung")
        
    # [단계6] 미래 예측(업무 적용)
        fore <- forecast(model) # 향후 2년 예측
        fore
        par(mfrow=c(1,2))
        plot(fore) # 향후 24개월 예측치 시각화 
        model2 <- forecast(model, h=6) # 향후 6개월 예측치 시각화 
        plot(model2)
        
    
        
        
## 2. 정상성시계열의 계절형 ##
    
    #[단계1] 시계열자료 특성분석
        
        
        #(1) 데이터 준비 
            data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 
                      55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 
                      56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
            plot(data)
            length(data)# 36
        
        #(2) 시계열자료 생성 
            tsdata <- ts(data, start=c(2016, 1), frequency=12)
            tsdata 
        
        #(3) 시계열요소분해 시각화
            ts_feature <- stl(tsdata, s.window="periodic")
            plot(ts_feature)
    
    
    #[단계2] 정상성시계열 변환
            
        # - 계절성을 갖는 경우에는 계절차분을 적용한다.
        
        par(mfrow=c(1,2))
        ts.plot(tsdata)
        diff <- diff(tsdata)
        plot(diff) # 차분 시각화
    
    
        
    #[단계3] 모형 식별과 추정
        
        # - 최적의 모형과 파라미터를 제공받기 위해서 auto.arima() 함수를 이용한다.   
        library(forecast)
        ts_model2 <- auto.arima(tsdata)  
        ts_model2
        # Series: tsdata
        # ARIMA(2,1,0)(1,0,0)[12]
        # 한번 차분한 IAR(2)
        # 계절 데이터 AR(1)
    
        
    #[단계4] 모형 생성 
        model <- arima(tsdata, order = c(2, 1, 0), 
                       seasonal = list(order = c(1, 0, 0)))
        model
    
    
    #[단계5] 모형 진단(모형 타당성 검정)
    
        # (1) 자기상관함수에 의한 모형 진단
            tsdiag(model)
        
        # (2)Box-Ljung에 의한 잔차항 모형 진단
            Box.test(model$residuals, lag=1, type = "Ljung")
    
    
    # [단계6] 미래 예측
        par(mfrow=c(1,2))
        fore <- forecast(model, h=24) # 2년 예측 
        plot(fore)
        fore2 <- forecast(model, h=6) # 6개월 예측 
        plot(fore2)
    
    
    
