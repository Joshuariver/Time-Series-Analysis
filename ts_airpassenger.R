rm(list=ls())
setwd("D:/RLab/Time Series/Time Series")

library(tseries)

# Time Series 데이터 인지 확인

data(AirPassengers)
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)

# 항공이용 승객의 숫자가 분포되어 있음.

plot(AirPassengers)

# 추세선을 그림

abline(reg=lm(AirPassengers~time(AirPassengers)))

# 년도별 데이터의 사이클을 보여줌

cycle(AirPassengers)

# 사이클을 감안한 매년의 평준화된 트랜드를 보여줌

plot(aggregate(AirPassengers,FUN=mean))

# 월별 Seasonality 효과를 박스 플롯으로 보여줌.

boxplot(AirPassengers~cycle(AirPassengers))

# 현재까지 결과로 본 중요한 추론(inference)들
# 매년 항공이용 승객의 숫자는 한 번의 축소 없이 증가하고 있음
# 7월과 8월의 분산과 평균이 다른 달에 비해서 현저히 높음.
# 매월 승객 수의 평균이 조금씩 다르지만 분산은 크게 차이는 없음. 그러므로 12개월로 분리해서 감안할 때 
# 강한 Seasonality 효과가 있음.

######################################################################
#
# 여기부터는ARMA 모델링을 통한 예측 입니다.  ARMA 분석의 순서는 아래와 같습니다.
# 
# 1. Time Series 를 먼저 본다.
# 2. 계열을 평준화 한다.
# 3. ACF/PACF 차트를 보고 최적의 매개변수를 찾는다.
# 4. ARMA 모델을 만든다.
# 5. 예측을 한다.
# 
######################################################################

# Time Series 를 먼저본다는 기존의 추세선을 포함한 plot을 보면 됨.

plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

# Augmented Dickey-Fuller Test 를 한다.
# 사용하는 Library 는 urca 혹은 tseries 패키지 이다.

adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

# P값이 0.05보다 적으므로, 이 데이터는 어떤 종류의 Time Series 분석을 하는 것도 가능하다는 것을 확인할 수 있음.
# 다음을 ACF/PACF 차트를 보고 ARMA 모델링을 위한 최적의 매개변수를 찾는 과정이다.

# ACF plot
acf(log(AirPassengers))
# 이 ACF차트로 보면 추세가 매우 완만하다는 것을 볼 수 있으며, 이는 모집단이 정적(stationary)이라는 것을
# 보여준다. 그러므로 추세해 대한 로그값을 직접 보기 보다는 로그값의 차계값을 관측한다.

acf(diff(log(AirPassengers)))

# PACF plot
pacf(diff(log(AirPassengers)))

# Auto Arima

auto.arima(ts(AirPassengers))

# Clearly, ACF plot cuts off after the first lag. Hence, we understood that value of p should be 0 
# as the ACF is the curve getting a cut off. While value of q should be 1 or 2. After a few iterations, 
# we found that (0,1,1) as (p,d,q) comes out to be the combination with least AIC and BIC.


# ARIMA 모델 구축
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

# 예측
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

