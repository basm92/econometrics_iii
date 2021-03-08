library(rio)
library(ggplot2)
library(dynlm) # estimate AR model
library(stargazer)
library(normtest)
setwd("~/Documents/econometrics_iii/Stas")
df <- import("data/data_assign_p1.csv")
df <- ts(df$GDP_QGR, frequency = 4, start = c(1987, 2))

#------------------------
# Part 1: Plot the Dutch GDP, ACF, and PACF
plot(df)
acf(df, lag.max = 12)
Box.test(df, lag = 12, type = "Ljung-Box")
pacf(df, lag.max = 12)

# Part 2: Estimate AR(p)
ar4 <- dynlm(df ~ L(df, 1) + L(df, 2) + L(df, 3) + L(df, 4))
ar3 <- dynlm(df ~ L(df, 1) + L(df, 3) + L(df, 4))
ar2 <- dynlm(df ~ L(df, 1) + L(df, 3))
ar1 <- dynlm(df ~ L(df, 1))
stargazer(ar4, ar3, ar2, ar1, type = "text")

ar1_m <- arima(df, c(1, 0, 0))
# only lag 1 is significant

# Part 3: Plot ACF of residuals
acf(ar1_m$resid, 12)

# Part 4: Forecast AR model for 2 years
df_pred <- predict(ar1_m, n.ahead = 8)$pred

# Part 5: Produce CI
df_ciu <- predict(ar1_m, n.ahead = 8)$pred + predict(ar1_m, n.ahead = 8)$se*1.96
df_cil <- predict(ar1_m, n.ahead = 8)$pred - predict(ar1_m, n.ahead = 8)$se*1.96

# Part 6: Check normality
jb.norm.test(ar1_m$resid)
# reject H0, innovations are not normally distributed

# Part 7



