
#------------------------------------------------------------------------------
# Typ: Skript
# Name: template-script. R
# Verfasser:Jonas Benischek
# Beschreibung: Setzen Sie die erforderlichen Variablen und rufen Sie die Einrichtung auf 
# Abhängigkeiten: geoAI_setup. R 
# Ausgabe: Liste der Pfade
# Urheberrecht: 2021, GPL (>= 3)
#------------------------------------------------------------------------------
# 0 - SpezifischesSetup
#-----------------------------


# MANDANTORY: Definieren des Stammordners Ändern Sie diese Zeile NICHT
rootDIR = "C:/Datenanalyse/GitHub/Repository/"


#require(envimaR)

#-- Weitere Anpassung des Setups durch den Benutzer in diesem Abschnitt 
#-- kann nur die Definition von Zusatzpaketen frei angepasst werden 
#-- und Verzeichnispfade MÜSSEN mit den beiden Variablen durchgeführt werden 
#-- appendpackagesToLoad und appendProjectDirList
#-- Fühlen Sie sich frei, diese Zeilen zu entfernen, wenn Sie sie nicht benötigen
# Definieren Sie zusätzliche Pakete Auskommentieren Sie ggf.
# appendpackagesToLoad = c("dummy-package")
# weitere Unterordner definieren Auskommentieren Sie ggf.
# appendProjectDirList =  c("data/dymmy-folder/")


# MANDANTORY: Aufruf des Setup-Skripts auch diese Zeile NICHT ändern
#source(file.path(envimaR::alternativeEnvi(root_folder = rootDIR),"src/geoAI_setup.R"),echo = TRUE)

# 1 - Skript starten
#-----------------------------


################################################################################
#
# Abgabe 1 GitHub
# Link: https://geomoer.github.io/moer-mpg-data-analysis/unit01/unit01-03_structures_vectors.html
#
############################################################################

# 

#Link Wichtig GitHub !!!!! Zur Abgabe der Hausaufgaben Kursordner mit Aktualisierung 

#https://github.com/GeoMOER-Students-Space/mpg-data-analysis-2022-Benische343

#https://github.com/GeoMOER-Students-Space/mpg-data-analysis-2022-Benische343.git

#Kurslink:
#  https://geomoer.github.io/moer-mpg-data-analysis/
  
############################################################################
############################################################################
# Abgabe 
#https://geomoer.github.io/moer-mpg-data-analysis/unit01/unit01-11_assignment.html




#1.Weisen Sie einer aufgerufenen Variablen den Wert fünf und einer aufgerufenen 
#Variablen den Wert zwei zu.

a <- c(5)    
b <- c(2)

#2.Berechnen Sie die Summe, die Differenz, das Produkt und das Verhältnis von a 
#und b (a immer an erster Stelle) und speichern Sie die Ergebnisse in vier 
#verschiedenen Variablen,,, die als und bezeichnet werden

5+2 #Summe
5-2 #Differenz
5*2 #Produkt
5/2 #Verhältnis

r1 <- c(7)
r2 <- c(3)
r3 <- c(10)
r4 <- c(2.5)


#3.Erstellen Sie einen Vektor, der die Werte enthält, die in den vier 
#Variablen aus Schritt 2 gespeichert sind.v1

v1 <- c(7,3,10,2.5)


#4.Fügen Sie einen fünften Eintrag zum Vektor hinzu, der durch die Potenz 
#von (d.h.) darstellt.

r5 <- c(22.5,5,2,5^2)

#5.Zeigen Sie den Inhalt des Vektors an (z. B. verwenden Sie die Funktion oder 
#geben Sie einfach den Variablennamen in eine separate Zeile ein)

print(v1)                  # Abgefrage Inhalt des Vektors v1


#6.Erstellen Sie einen zweiten Vektor, der Informationen über die Art der 
#mathematischen Operation enthält, die zur Ableitung der fünf Ergebnisse 
#verwendet wird. Daher sollte dieser Vektor fünf Einträge von Werten sum, 
#Differenz haben





#7. Zeigen Sie den Inhalt des Vektors an

print(v2)


#8.Kombinieren Sie die beiden Vektoren in einen Datenrahmen mit dem Namen. 
#Jeder Vektor sollte eine Spalte des Datenrahmens werden, so dass Sie einen 
#Datenrahmen mit 5 Zeilen und 2 Spalten erhalten.v1v2df



#9. Stellen Sie sicher, dass die Spalte mit den Datenvon Results und Operation 
#heißt.v1v2


#10 Zeigen Sie den gesamten Inhalt von an.df

print(df)

#11 Zeigt nur den Eintrag der Zelle in der zweiten Zeile und ersten Spalte an.




###########################################################################
##  Vektoren  --------------------------------------------------
###########################################################################

## Vektoren sind eindimensionale Sammlungen "atomarer Objekte".
## Sie entstehen somit durch die Kombination einzelner Variablen.
## WICHTIGE EINSCHRAENKUNG: Alle Elemente eines Vektors muessen derselben 
## Datenklasse (z.B. numeric) angehoeren!


## Vektoren erstellen - die c()-Funktion = Kombinieren
u <- c(-107, 114, -326, 412)                        # Vektor u: Zusammenstellung aus 4 Zahlen
u
v <- c("Anna", "Julia", "Michael", "Christian")     # Vektor v: Zusammenstellung aus 4 Strings
v


## Werden unterschiedliche Datenklassen miteinander kombiniert,
## so versucht R, das Ergebnis sinnvoll zu interpretieren. 
## Das fuehrt manchmal zu nicht gewollten Ergebnissen.
w <- c(100, 200, "Test")                            # Kombination aus Zahlen und Text
w                                                   # Wird von R als reiner Text-Vektor interpretiert!


## Vektoren erstellen - Zahlenfolgen
z <- 1:10                                           # Vektor z: Ganze Zahlen von 1 bis 10
z
zz <- c(20:30, 60:70)                               # Vektor zz: Ganze Zahen von 20 bis 30 und von 60 bis 70
zz


## Vektoren erstellen - Wiederholungen
help(rep)

Wiederholung_1 <- rep(3, times = 8)
Wiederholung_1

Wiederholung_2 <- rep(3, times = 800)
Wiederholung_2
length(Wiederholung_2)                              # Laenge eines Objektes bestimmen

Wiederholung_3 <- rep(-10:5, times = 15)
Wiederholung_3

Wiederholung_4 <- rep(3:9, each = 4, times = 6)
Wiederholung_4

Wiederholung_5 <- rep("Das ist die Zahl", times = length(Wiederholung_2))
Wiederholung_5


## Vektoren erstellen - Sequenzen
help(seq)

Sequenz_1 <- seq(from = 100, to = 300, by = 10)       ## Langform - empfehlenswert
Sequenz_1

Sequenz_2 <- seq(10,70,5)                             ## Kurzform - nur bedingt gut
Sequenz_2

Sequenz_3 <- seq(20,30,3)
Sequenz_3  



## Vektoren abfragen - allgemein
v                 # Vektor v wird komplett abgefragt
v[2]              # 2. Element im Vektor v
v[c(2,4)]         # 2. und 4. Element im Vektor v
v[-2]             # Vektor v, aber ohne das 2. Element
zz[9:12]          # 9. bis 12. Element des Vektors zz

length(v)         # Laenge von Vektor v (wie viele Elemente enthaelt der Vektor?)
length(zz)

max(u)            # Maximum des Vektors u
min(u)            # Minimum des Vektors u
summary(zz)


## Einschraenkende Abfragen - Die Funktionen which() und subset()
## In der Regel moechte man Datensaetze nach spezifischen Kriterien auswerten.
## Dazu muessen die entsprechenden Daten zunaechst im Hinblick auf dieses 
## Kriterium selektiert werden. Hierzu werden logische Abfragen (vgl. oben!)
## in Verbindung mit speziellen Funktionen eingesetzt.

## Zur Funktionsweise der which()-Funktion:
v
v != "Michael"                  # Logische Abfrage (vgl. oben)
which(v != "Michael")           # Anwendung der which()-Funktion; 
# Bestimmung der Positionen, fuer welche 
# der Werte TRUE ermittelt wurde

v_ohne_Michael.1 <- v[which(v != "Michael")]
v_ohne_Michael.1


## Zur Funktionsweise der subset()-Funktion
help(subset)
v_ohne_Michael.2 <- subset(x = v, subset = v != "Michael")
v_ohne_Michael.2


## Zur Funktionsweise der unique()-Funktion
length(Wiederholung_3)          # Laenge des Vektos Wiederholung_3 (alle Elemente)
Wiederholung_3
unique(Wiederholung_3)          # Welche unterschiedlichen Elemente gibt es - ohne die Wiederholungen?
length(unique(Wiederholung_3))  # Laenge der unterschiedlichen Elemente - wie viele verschiedene Elemente gibt es?


## Und wieder etwas aufraeumen
rm(list = ls())












