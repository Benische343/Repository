

#------------------------------------------------------------------------------
# Unit:10
# Verfasser:Jonas Benischek
# Beschreibung: ZEITREIHEN ANALYSIEREN
#------------------------------------------------------------------------------

# MANDANTORY: Definieren des Stammordners Ändern Sie diese Zeile NICHT
rootDIR = "C:/Datenanalyse/UsingR/Unit10/"

##############################################################################
# Link: https://geomoer.github.io/moer-mpg-data-analysis/unit10/unit10-04_assignment.html


##########################################################################################
#NAO und Lufttemperatur in Cölbe
#Die benötigten Stationsdaten können direkt aus dem Deutscher Wetterdienst 
#(DWD) über seinen Server. Neben Cölbe finden Sie hier auch alle verfügbaren
#Klimastationen für Deutschland.

# 03164    Cölbe, Kr. Marburg-Biedenkopf            Hessen  


#Öffnen über Notepad++

df <- read.table("/Users/Jonas/Documents/Datenanalyse/UsingR/Unit10/Metadaten_Geraete_Lufttemperatur_03164.txt",
                 , header = TRUE, sep = ";", dec = ",",encoding = "latin1", 
                 nrows = 12) #Zeile 8930 löschen um die Fehlermelung zu umgehen

str(df) #Ansicht in der Console


head(df)


# acf(df$LUFTTEMPERATUR, lag.max = 100)

#df$AGG_JM <- substr(df$MESS_DATUM, 1, 6)
#tam <- aggregate(dwd$LUFTTEMPERATUR, by = list(df$AGG_JM), FUN = mean)
#colnames(tam) <- c("Date", "Ta")
#acf(tam$Ta)
#pacf(tam$Ta)

#tam <- tam[-(1:6),]
#tam$Date <- strptime(paste0(tam$Date, "010000"), format = "%Y%m%d%H%M", tz = "UTC")
#plot(tam$Date, tam$Ta, type = "l")


#spec <- spectrum(tam$Ta)

#plot(1/spec$freq, spec$spec, type = "h")






