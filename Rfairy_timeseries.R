############################################################
### 수업내용 : Time Series                               ###
### 작성자 : Adonis Han.                                 ###
### 작성일자 : 2017.08.03 (목)                           ###
############################################################

# 규칙적 / 불규칙적.
# 시간 ㅡ 관측치. 
# 과거 시계열 자료의 패턴이 유지된다는 가정 -> 과거를 바탕으로 미래를 예측
# (1) AR / MA
# -자기 상관 (Autocorrelation, AR): 이전 값이 이후에 영향, 평균으로 회귀.
# -이동 평균 (Moving average, MA): 이전에 발생한 값들 사이의 차이
# -지수 평활(Exponential smoothing) : 최근 관측치와 최근 예측치의 평균으로 예측

# (3)
# VAR 계열: 시계열 간 상관관계 - 서로가 서로의 설명변수가 된다. 
# (4)
# GARCH 계열: 편차가 큰 시계열 (주로 Finance)

# 안정적으로 해야한다 : Stationary Time Series ??? 평균, 분산의 일관성

# Stationary 하지 않다면?
#차분 (diff) /로그 (log) 아니면 Auto ARIMA 사용. 

# Progress
# 1.자료 Plotting 및 분석 방법 선택 
# 2.시계열 자료로 변환 (stationary 만족 여부 확인)
# 3.모델링 및 예측

# 아리마계열 methodology
# ARMA (Autoregressive moving average) - AR + MA 합한 것  
# ARIMA (Autoregressive integrated moving average) - Co-integration 개념 추가, 모멘텀 반영 
# 추세와 모멘텀을 반영.

# example : autoarima : https://stackoverflow.com/questions/34802385/auto-arima-calculates-a-model-that-cannot-predict

# 데이터사용 : AirPassengers 
str(AirPassengers)
# (1) 그래프로 그려본다.
plot(AirPassengers)
plot(stl(AirPassengers, s.window="periodic"))

# (2) 자기상관
par(mfrow=c(1,2))
acf(AirPassengers)
pacf(AirPassengers)

library(forecast)
ndiffs(x=AirPassengers)
# 출력  1
# 몇 번을 차분해야 안정적인 시계열을 얻을수 있는지 알려준다.
par(mfrow=c(1,1)) #한 줄 한 칸.
plot(diff(AirPassengers,1))
plot(diff(log(AirPassengers),1))
Airs<-diff(log(AirPassengers),1)

# (3) 모델링 & 예측. 
library(tseries)
adf.test(Airs, alternative = "stationary", k=0)

Airbest<-auto.arima(x=AirPassengers)
Airbest

predict(Airbest, n.ahead=5, se.fit=TRUE) 

forecast(Airbest, h=40) %>%
  plot()


# ------------------ARIMA 실습 ??? WDI 패키지  (GDP자료)
# World bank API 패키지 설치 (“WDI”)
# GDP 데이터


library(WDI)
gdp<-WDI(country = c("US", "CA", "GB", "CN", "JP", "SG", "IL", "KR"),
         indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
         start=1960, end=2016
)

#PerCapGDP : GDP (nominal) per capita - 1인당 GDP
names(gdp)<-c("iso2c", "Country", "Year", "PerCapGDP", "GDP")
View(gdp)
head(gdp)
library(ggplot2)
library(scales)

ggplot(gdp, aes(Year, PerCapGDP, color=Country, linetype=Country)) + 
  geom_line()+scale_y_continuous(label=dollar)


#ts는 WDI함수. 
us<-gdp$PerCapGDP[gdp$iso2c=="US"] # short country name 의 이름이 US 인 것에서 PercapGDP
us<-ts(us)  #ts는 time series 
us<-ts(us, start = c(1960, 1), end=c(2016, 1), frequency = 1) #시작연도, 종료년도, 주기. 시차타임이 중요하기때문에, 
plot(us, ylab="Per Capita GDP", xlab="Year")

par(mfrow=c(1,2))
acf(us)
pacf(us)
ndiffs(us)

usBest<-auto.arima(x=us)
usBest 

# Series:  
#   ARIMA(1,2,1)                    

# Coefficients:
#   ar1      ma1
# 0.4311  -0.8841
# s.e.  0.1456   0.0660
# -자기 상관 (Autocorrelation, AR): 이전 값이 이후에 영향, 평균으로 회귀.
# -이동 평균 (Moving average, MA): 이전에 발생한 값들 사이의 차이

# sigma^2 estimated as 286796:  log likelihood=-422.99
# AIC=851.97   AICc=852.44   BIC=857.99

par(mfrow=c(1,1))
predict(usBest, 5) #파란색이 예측한다는것. 80~95% 신뢰구간 
forecast(usBest, h=5)
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95 
# 2017       58757.18 58070.87 59443.50 57707.56 59806.81 #실제로 예측한 값을 것이다.  #Low 95신뢰구간.  57~59 sjfqrh.
# 2018       60060.79 58796.55 61325.03 58127.30 61994.27
# 2019       61370.09 59554.50 63185.69 58593.38 64146.81
# 2020       62681.85 60335.23 65028.48 59093.00 66270.70
# 2021       63994.67 61128.69 66860.65 59611.53 68377.81
usforecast = forecast(usBest, h=5)
plot(usforecast)


par(mfrow=c(1,1))
forecast(usBest, h=5)->usforecast
plot(usforecast)
