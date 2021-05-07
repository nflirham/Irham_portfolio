#PEMODELAN
library(tseries)
library(TSA)
library(forecast)
library(lmtest)
library(FinTS)

#input data
data <- read.csv("D:/Kuliah/Semester 4/AK2281 - Analisis Deret Waktu/Tugas Besar/Share COWL.JK (2).csv", 
                 head=T, sep=",",dec = ".")

## Create a time series object
saham <- ts(data$Open)

logsaham = log(saham)
#1. Plot data dan Uji kestasioneran

plot(saham, main = "Plot data saham mandiri Cowl.JK")
#ggseasonplot(saham)
#ggsubseriesplot(saham)

Acf(saham)
Pacf(saham)

adf.test(saham)

#Diferensi original
diff = diff(saham)
plot(diff, main="Differensi pertama data saham")

adf.test(diff)
auto.arima(saham)

diff1 = diff(diff(saham))
plot(diff1, main = "d=2")
adf.test(diff1)

#3. Identifikasi orde
Acf(diff)
Pacf(diff)

#Identifikasi model manual dari pacf dan acf

(model1 = Arima(saham, order = c(0,1,2)))
(model2 = Arima(saham, order = c(1,1,0)))
(model3 = Arima(saham, order = c(1,1,1)))
(model4 = Arima(saham, order = c(1,1,2)))
auto.arima(saham)


coeftest(model2)

#Uji Diagnostik
checkresiduals(model2)

ArchTest(residuals(model2))


#5. Fitting model
plot(saham)
lines(fitted(model1),col = "red")

#6. Forecasting

fc = forecast(model1, h =12)
fc
plot(fc, main = "forecasting 12 bulan kedepan")


