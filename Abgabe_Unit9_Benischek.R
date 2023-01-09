

#------------------------------------------------------------------------------
# Unit:9
# Abgabe: 9
# Verfasser:Jonas Benischek
# Beschreibung: Prognostizieren Sie Ihre zeitlichen Daten
#------------------------------------------------------------------------------

# MANDANTORY: Definieren des Stammordners Ändern Sie diese Zeile NICHT
rootDIR = "C:/Datenanalyse/UsingR/Unit9/"

##############################################################################
# Prognostizieren Sie Ihre zeitlichen Daten
# Link: https://geomoer.github.io/moer-mpg-data-analysis/unit09/unit09-01_predict_time_series.html


#############################################################################
# Daten:
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/historical/
#
# 03164 20060701 20230109            187     50.8492    8.7745 Cölbe, Kr. Marburg-Biedenkopf            Hessen  
#

df <- read.table("/Users/Jonas/Documents/Datenanalyse/UsingR/Unit9/produkt_rr_stunde_20060701_20211231_03164.txt",
                 skip = 4, header = TRUE, sep = ";", dec = ",",encoding = "latin1")

str(df) #Ansicht in der Console

head(df)


#'data.frame':	135473 obs. of  7 variables:
#  $ X3164      : int  3164 3164 3164 3164 3164 3164 3164 3164 3164 3164 ...
#$ X2006070103: int  2006070104 2006070105 2006070106 2006070107 2006070108 2006070109 2006070110 2006070111 2006070112 2006070113 ...
#$ X3         : int  3 3 3 3 3 3 3 3 3 3 ...
#$ X0.0       : chr  "   0.0" "   0.0" "   0.0" "   0.0" ...
#$ X0         : int  0 0 0 0 0 0 0 0 0 0 ...
#$ X.999      : int  0 0 -999 0 0 -999 0 0 -999 0 ...
#$ eor        : chr  "eor" "eor" "eor" "eor" ...



##########################################################################
#Autoregressive Modelle (AR)

dTa <- diff(diff(tam$Ta))
acf(dTa)

armod <- ar(dTa, aic = TRUE, order.max = 20, method = "yule-walker")
armod

par_org <- par()
par(mfrow = c(1,2))
acf(dTa)
pacf(dTa)

armod <- ar(dTa, aic = TRUE, order.max = 20, method = "mle")
armod

arpred <- predict(armod, n.ahead = 100)

plot(dTa, type = "l", xlim = c(0, length(dTa)+100))
lines(arpred$pred, col = "red")
lines(arpred$pred + arpred$se, col = "grey")
lines(arpred$pred - arpred$se, col = "grey")


##########################################################################
# Autoregressive integrierte gleitende Durchschnittsmodelle (ARIMA)


armod <- arima(tam$Ta, order = c(17,2,0), method = "ML")  # the order is p, d, q
armod

arimamod <- arima(tam$Ta, c(6,2,2))
summary(arimamod)

arima_predict <- predict(arimamod, n.ahead = 100)

plot(tam$Ta, type = "l", xlim = c(0, length(dTa)+100))
lines(arima_predict$pred, col = "red")
lines(arima_predict$pred + arima_predict$se, col = "grey")
lines(arima_predict$pred - arima_predict$se, col = "grey")

arima_s <- auto.arima(tam_ts, max.p = 20, max.q = 20, seasonal=TRUE)
summary(arima_s)
