

#Unit2

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
rootDIR = "C:/Datenanalyse/UsingR/Unit2/"

#Konvertieren von Datentypen
#https://geomoer.github.io/moer-base-r/unit02/unit02-06_convert.html


#require(envimaR)

value <- 23.5

is.numeric(value)

is.character(value)

color <- c("blue","red","red","yellow")
color


is.character(color)


color.factor <- as.factor(color)
color.factor

#Levels: blue red yellow
#Das Setzen des Großbuchstabens "L" nach einer ganzen Zahl erzwingt, dass es als ganze Zahl gespeichert wird.

is.numeric(1)


is.integer(1)


is.numeric(1L)


is.integer(1L)


#Objekttypen
#https://geomoer.github.io/moer-base-r/unit03/unit03-03_types_of_objects.html



#Indizierung
#https://geomoer.github.io/moer-base-r/unit04/unit04-02_index.html


# bsp. Generating some data
v <- 10:15
#Sie können auch einen Index verwenden, um Werte zu ändern

v[1] <- 11 # Change position 1 from 10 to 11
v #Anzeigen


#Werte aussotieren

x <- 10:20
b <- x > 15
b   # Anzeigen welche werte trifft was zu
x[b]  # Nur werte ausgewählt größer als 15


#oder mit der which Funktion arbeiten

x <- 10:20
j <- c(7,9,11,13)
j %in% x

## [1] FALSE FALSE  TRUE  TRUE

which(j %in% x)
## [1] 3 4
#Eine weitere praktische ähnliche Funktion ist Match:
  
  match(j, x)


  #subset function für Abfragen
  
  manual3 <- subset(mtcars, am == 1, select = c(mpg, hp, gear))
  
  #           subset....objektname mit einer Bedingung...... welche Spalten

#################################################################################
 # Operatoren
#https://geomoer.github.io/moer-base-r/unit07/unit07-02_operators.html
  
 #Arithmetische Operatoren	 	 
 # +	Addition	x + y
 # -	Subtraktion	x - y
#*	Multiplikation 
 # usw.
  
###########################################################################
#Loop mit der if Funktion
#  https://geomoer.github.io/moer-base-r/unit07/unit07-03_decisions_loops.html 

  
  #For Schleife erstellen bsp.
  
  a <- seq(7,10) 
  for(j in a){print(j)}
  

  for(i in seq(7, 10)){
    print(paste0("Outer loop value of a: ", i))
    
    if(i < 10){
      lower_border <- i + 1  #Zeile neues opjekt wird hinzugefügt'
    } else {
      lower_border <- i  #wird ausgeführt wenn die zeile mit dem +1 nicht zutrifft
    }
    for(j in seq(10, lower_border)){    #zweiter Loop
      print(paste0("   Inner loop value of c: ", j))
    }
  }  #Die KLammern sollen so wie hier stehen
  
  
  #Graphische Ausgebe in der Console wie sich i und j verändert
  
###############################################################################
  a <- c("A", "B", "C", "D")
  for(i in seq(length(a))){
    a[i] <- paste0(a[i], "x") #x wird über die paste funktion zu a und i hinzugefügt
  }
  
    
    
    

