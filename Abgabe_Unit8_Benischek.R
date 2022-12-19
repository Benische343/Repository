

#------------------------------------------------------------------------------
# Unbit:8
# Abgabe: 8
# Verfasser:Jonas Benischek
# Beschreibung: Modelloptimierung 
#------------------------------------------------------------------------------

# MANDANTORY: Definieren des Stammordners Ändern Sie diese Zeile NICHT
rootDIR = "C:/Datenanalyse/UsingR/Unit8/"

##############################################################################
# Modell Tuning
# Link: https://geomoer.github.io/moer-mpg-data-analysis/unit08/unit08-03_assignment.html


#############################################################################
# Für die Implementierung eines Kreuzvalidierungs-basierten und recht einfachen 
#Tunings eines additiven Modells greifen wir auf den Datensatz der Holzernte zurück. 
#Ziel ist es, die Buchenernte aus der Eichenernte vorherzusagen.

#Schreiben Sie ein R-Markdown-Skript, das die Vorhersage der Buchenernte basierend 
#auf der Eichenernte mithilfe eines additiven Modells abstimmt. 
#Das Tuning sollte über die Anzahl der Knoten zwischen 3 und 13 laufen, 
#Und die Schätzung der Modellleistung für jeden Knoten sollte auf einer 
#100-fachen Kreuzvalidierung basieren, wobei 80 Prozent des Datensatzes als Training 
#und der Rest als Testproben verwendet werden. Die Kreuzvalidierung sollte 
#reproduzierbar sein. Bitte visualisieren i) der mittlere quadratische Fehler, 
#(ii) der mittlere quadratische Fehler +/- es ist die Standardabweichung und (iii) 
#das angepasste R-Quadrat als Funktion zunehmender Knoten (d. h. Knoten sollten auf 
#der x-Achse liegen). Da der quadratische Mittelwert viel größer ist als die 
#R-Quadratwerte, normalisieren Sie den quadratischen Mittelwert für die Visualisierung 
#auf sein Maximum.

df <- read.table("/Users/Jonas/Documents/Datenanalyse/UsingR/Unit8/hessen_holzeinschlag_1997-2014.csv",
                 skip = 4, header = TRUE, sep = ";", dec = ",",encoding = "latin1")

str(df) #Ansicht in der Console



######################################################
######################################################

summary.data.frame(df) #Zusammenfassung in der Console

#Visualisierung mit einem Boxplot von allen Baumtypen

print(df) #Zeilen 2 bis 3 müssen ausgewählt werden

#Visualisierung mit einem Boxplot für alle Baumtypen
plot(df[, 2:3]) #Zeilen 2 bis 6 auswählen

##############################################

install.packages("ggplot2") #Diagramm für Gruppen und Untergruppen

library(ggplot2)

###################################################################################################

install.packages("Rmisc")

library(Rmisc)



###########################################################################################
#Multiples Liniendiagramm Punkte


ggplot()+
  ggtitle("Der Holzeinschlag in Hessen von 1997-2014")+
  
  geom_point(data = df, mapping = aes(x=FWJ, y=Buche), color="green")+     #Buchen in grün
  geom_point(data = df, mapping = aes(x=FWJ, y=Eiche), color="red")+       #Eichen in rot
  
  labs(x = "Jahr", y = "Menge: Eiche=Rot & Buche=Grün ", color = "Legend")

#################################################################################################
#Multiples Liniendiagramm 

ggplot()+
  ggtitle("Der Holzeinschlag in Hessen von 1997-2014")+
  
  geom_point(data = df, mapping = aes(x=FWJ, y=Buche), color="green")+     #Buchen in grün
  geom_point(data = df, mapping = aes(x=FWJ, y=Eiche), color="red")+       #Eichen in rot
  
  geom_line(data = df, mapping = aes(x=FWJ, y=Buche), color="green")+     #Buchen in grün
  geom_line(data = df, mapping = aes(x=FWJ, y=Eiche), color="red")+       #Eichen in rot
  
  labs(x = "Jahr", y = "Menge: Eiche=Rot & Buche=Grün ", color = "Legend")

############################################################################
############################################################################


library(mgcv)




Buche <- anscombe$y2
x1 <- anscombe$x1

set.seed(2)
x3 <- anscombe$x3 + sample(seq(-1, 1, 0.1), nrow(anscombe))

Buche <- c(anscombe$y1, anscombe$y2)
Eiche <- c(x1, x3)
plot(Eiche, Buche)

############################

df <- data.frame(y = Buche,
                 x = Eiche)

############################

plot(df$x, df$y)

lmod <- lm(y ~ x, data = df)
abline(lmod, col = "red")

############################

summary(lmod)

# Pr(>|t|)  0.000942 ***

# p-value: 9.676e-07

####################

####################
#Die Zusammenfassung:

gammod <- gam(y ~ x, data = df, familiy = gaussian())

px <- seq(min(df$x), max(df$x), 0.1)
gampred <- predict(gammod, list(x = px))

plot(df$x, df$y)
abline(lmod, col = "grey")
lines(px, gampred, col = "red", lty=2)

###############

summary(gammod)


#Deviance explained = 70.7%

############################


############################
#Kurve leicht gebogen 

gammod <- gam(y ~ s(x, fx = FALSE), data = df)

px <- seq(min(df$x), max(df$x), 0.1)
gampred <- predict(gammod, list(x = px))

plot(df$x, df$y)
lines(px, gampred, col = "red")

##############

summary(gammod)

# Deviance explained = 77.5%

##########################################

gam.check(gammod)

##########################################

plot(gammod)

#########################################

gammod <- gam(y ~ s(x, bs = "tp", fx = TRUE), data = df)

px <- seq(min(df$x), max(df$x), 0.1)
gampred <- predict(gammod, list(x = px))

plot(df$x, df$y)
lines(px, gampred, col = "red")

################

summary(gammod)

#Deviance explained = 90.4%

############################

knots <- seq(3, 13)

palette <- colorRampPalette(colors=c("blue", "green", "red"))
cols <- palette(length(knots))

plot(df$x, df$y)

for(i in seq(length(knots))){
  gammod <- gam(y ~ s(x, k = knots[i], fx = TRUE), data = df)
  px <- seq(min(df$x), max(df$x), 0.1)
  gampred <- predict(gammod, list(x = px))
  lines(px, gampred, col = cols[i], lty=2)
}

legend(13, 7.5, paste("knots", knots, sep = " "), col = cols, lty=2, cex=0.75)

##############################################################################

loessmod <- loess(y ~ x, data = df, span = 0.75)

px <- seq(min(df$x), max(df$x), 0.1)
loesspred <- predict(loessmod, data.frame(x = px), type = "response")

plot(df$x, df$y)
lines(px, loesspred, col = "red")

#################################

window <- seq(0.3, 1, 0.01)

palette <- colorRampPalette(colors=c("blue", "green", "red"))
cols <- palette(length(window))

plot(df$x, df$y)

for(i in seq(length(window))){
  loessmod <- loess(y ~ x, data = df, span = window[i])
  px <- seq(min(df$x), max(df$x), 0.1)
  loesspred <- predict(loessmod, data.frame(x = px))
  lines(px, loesspred, col = cols[i])
}






