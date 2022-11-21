

#------------------------------------------------------------------------------
# Unbit:4
# Abgabe: 4
# Verfasser:Jonas Benischek
# Beschreibung: Daten aufräumen
#------------------------------------------------------------------------------

# MANDANTORY: Definieren des Stammordners Ändern Sie diese Zeile NICHT
rootDIR = "C:/Datenanalyse/UsingR/Unit4/"

##############################################################################
# Das Bereinigen von Erntedaten
# Wird nicht benotet
# Link: https://geomoer.github.io/moer-mpg-data-analysis/unit04/unit04-07_assignment.html


##########################################################################################
# Bitte schauen Sie sich die Daten an und erstellen Sie eine Liste von Aufgaben, die     #
# Sie bewältigen müssen, um die Daten zu bereinigen.                                     #
##########################################################################################

#Öffnen über Notepad++

df <- read.table("/Users/Jonas/Documents/Datenanalyse/UsingR/Unit4/feldfruechte.txt",
                 skip = 1, header = TRUE, sep = ";", dec = ",",encoding = "latin1", 
                 nrows = 8930) #Zeile 8930 löschen um die Fehlermelung zu umgehen

str(df) #Ansicht in der Console



###################################################
# 1.Liste der Aufgaben un die Daten zu bereinigen #
###################################################
# 
#Aufgaben Liste:
#
# 1.1 Werte ohne nenenswerten Inhalt filtern 
# 1.2 Das umbennen der Spalten 
# 1.3 Das Konvertieren von Datentypen von chr zu numeric
# 1.4 Die Na Werte Löschen

#############################################################################################
# Bearbeitung der Aufgaben
##########################################################################################

############################################
# 1.1 Werte ohne nenenswerten Inhalt filtern

is.na(df) #Um zu überprüfen, ob der Vektor einen oder mehrere "nicht verfügbare" Werte hat

# Ergebnis in dem Datensatz sind keine NA Werte vorhanden


###########################################
# 1.2 Das umbennen der Spalten 

names(df) <- c("Jahr", "Plz", "Region", "Winterweizen", "Roggen und Wintermenggetreide", 
               "Wintergerste", "Sommergerste","Hafer", "Triticale", "Kartoffeln", 
               "Zuckerrüben", "Winterraps", "Silomais")
str(df)



########################################################
# 1.3 Das Konvertieren von Datentypen von chr zu numeric

for(c in colnames(df)[4:7]){
  df[, c][df[, c] == "."] <- NA  #jede Zelle, die nur ein "." enthält, auf NA abändern
  df[, c] <- as.numeric(sub(",", ".", as.character(df[, c])))
}

str(df)

################################
# 1.4 Na Werte Löschen


data_clean <- na.omit(df) 

str(data_clean)






