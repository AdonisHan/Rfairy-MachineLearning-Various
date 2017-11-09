############################################################
### �������� : Time Series                               ###
### �ۼ��� : Adonis Han.                                 ###
### �ۼ����� : 2017.08.03 (��)                           ###
############################################################

# ��Ģ�� / �ұ�Ģ��.
# �ð� �� ����ġ. 
# ���� �ð迭 �ڷ��� ������ �����ȴٴ� ���� -> ���Ÿ� �������� �̷��� ����
# (1) AR / MA
# -�ڱ� ��� (Autocorrelation, AR): ���� ���� ���Ŀ� ����, ������� ȸ��.
# -�̵� ��� (Moving average, MA): ������ �߻��� ���� ������ ����
# -���� ��Ȱ(Exponential smoothing) : �ֱ� ����ġ�� �ֱ� ����ġ�� ������� ����

# (3)
# VAR �迭: �ð迭 �� ������� - ���ΰ� ������ ���������� �ȴ�. 
# (4)
# GARCH �迭: ������ ū �ð迭 (�ַ� Finance)

# ���������� �ؾ��Ѵ� : Stationary Time Series ??? ���, �л��� �ϰ���

# Stationary ���� �ʴٸ�?
#���� (diff) /�α� (log) �ƴϸ� Auto ARIMA ���. 

# Progress
# 1.�ڷ� Plotting �� �м� ��� ���� 
# 2.�ð迭 �ڷ�� ��ȯ (stationary ���� ���� Ȯ��)
# 3.�𵨸� �� ����

# �Ƹ����迭 methodology
# ARMA (Autoregressive moving average) - AR + MA ���� ��  
# ARIMA (Autoregressive integrated moving average) - Co-integration ���� �߰�, ����� �ݿ� 
# �߼��� ������� �ݿ�.

# example : autoarima : https://stackoverflow.com/questions/34802385/auto-arima-calculates-a-model-that-cannot-predict

# �����ͻ�� : AirPassengers 
str(AirPassengers)
# (1) �׷����� �׷�����.
plot(AirPassengers)
plot(stl(AirPassengers, s.window="periodic"))

# (2) �ڱ���
par(mfrow=c(1,2))
acf(AirPassengers)
pacf(AirPassengers)

library(forecast)
ndiffs(x=AirPassengers)
# ���  1
# �� ���� �����ؾ� �������� �ð迭�� ������ �ִ��� �˷��ش�.
par(mfrow=c(1,1)) #�� �� �� ĭ.
plot(diff(AirPassengers,1))
plot(diff(log(AirPassengers),1))
Airs<-diff(log(AirPassengers),1)

# (3) �𵨸� & ����. 
library(tseries)
adf.test(Airs, alternative = "stationary", k=0)

Airbest<-auto.arima(x=AirPassengers)
Airbest

predict(Airbest, n.ahead=5, se.fit=TRUE) 

forecast(Airbest, h=40) %>%
  plot()


# ------------------ARIMA �ǽ� ??? WDI ��Ű��  (GDP�ڷ�)
# World bank API ��Ű�� ��ġ (��WDI��)
# GDP ������


library(WDI)
gdp<-WDI(country = c("US", "CA", "GB", "CN", "JP", "SG", "IL", "KR"),
         indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
         start=1960, end=2016
)

#PerCapGDP : GDP (nominal) per capita - 1�δ� GDP
names(gdp)<-c("iso2c", "Country", "Year", "PerCapGDP", "GDP")
View(gdp)
head(gdp)
library(ggplot2)
library(scales)

ggplot(gdp, aes(Year, PerCapGDP, color=Country, linetype=Country)) + 
  geom_line()+scale_y_continuous(label=dollar)


#ts�� WDI�Լ�. 
us<-gdp$PerCapGDP[gdp$iso2c=="US"] # short country name �� �̸��� US �� �Ϳ��� PercapGDP
us<-ts(us)  #ts�� time series 
us<-ts(us, start = c(1960, 1), end=c(2016, 1), frequency = 1) #���ۿ���, ����⵵, �ֱ�. ����Ÿ���� �߿��ϱ⶧����, 
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
# -�ڱ� ��� (Autocorrelation, AR): ���� ���� ���Ŀ� ����, ������� ȸ��.
# -�̵� ��� (Moving average, MA): ������ �߻��� ���� ������ ����

# sigma^2 estimated as 286796:  log likelihood=-422.99
# AIC=851.97   AICc=852.44   BIC=857.99

par(mfrow=c(1,1))
predict(usBest, 5) #�Ķ����� �����Ѵٴ°�. 80~95% �ŷڱ��� 
forecast(usBest, h=5)
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95 
# 2017       58757.18 58070.87 59443.50 57707.56 59806.81 #������ ������ ���� ���̴�.  #Low 95�ŷڱ���.  57~59 sjfqrh.
# 2018       60060.79 58796.55 61325.03 58127.30 61994.27
# 2019       61370.09 59554.50 63185.69 58593.38 64146.81
# 2020       62681.85 60335.23 65028.48 59093.00 66270.70
# 2021       63994.67 61128.69 66860.65 59611.53 68377.81
usforecast = forecast(usBest, h=5)
plot(usforecast)


par(mfrow=c(1,1))
forecast(usBest, h=5)->usforecast
plot(usforecast)