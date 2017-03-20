### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 3: Dateien und Arbeitsverzeichnis ##################################

# 3.1 Arbeitsverzeichnis 
# 3.2 Tabellen einlesen 
# 3.3 Anzeigen ausgew�hlter Elemente
# 3.4 attach
# 3.5 Tabelle exportieren (schreiben)
# 3.6 Oft verwendeter Operatoren
# 3.7 Excel-Dateien lesen und schreiben


# 3.1 Arbeitsverzeichnis (Working Directory) -----------------------------------
# An diesem Ort (Ordner) werden Dateien gelesen und gespeichert, sofern nicht 
# der ganze Pfad angegeben wird. Dadurch kann ein Ordner mit Dateien und Skript
# einem Kollegen gegeben werden, ohne das der alle Pfade �ndern muss.
# Schnelles und reproduzierbares Einstellen mit dem Befehl setwd:
setwd("D:/Ordner/BerryIstToll/Rclick_Handbuch")
# alle \ umwandeln in / oder \\. Bei langen Pfaden im TinnR STRG + R nutzen!
# ( \ ist in R die EscapeSequenz aus einer Zeichenkette heraus)
# Tipp: Im Windows Explorer Datei markieren, F4 dr�cken um den Pfad zu kopieren.
# Tipp: W�hle sinnvollerweise gut zug�ngliche Ordner aus.

# Alternativ per Maus, aber anderen Skriptlesern dann nicht ersichtlich: 
# In der R Console "Datei" klicken, dann "Verzeichnis wechseln". 

# �berpr�fen des aktuellen Verzeichnisses mit
getwd()

# Wenn ein R-Skript in Tinn-R ge�ffnet wird, und danach R selbst mit Tinn-R
# ge�ffnet wird, ist das Arbeitsverzeichnis automatisch der Ordner, in dem die
# R-Datei sich befindet. Wenn also die Daten im gleichen Ordner wie das R-Skript
# vorliegen, entf�llt die meiste Arbeit mit dem Working Directory.

# Anzeigen der Datei(en) im Arbeitsverzeichnis
dir()                    # �bersicht der Dateien und Ordner (keine Unterordner)
dir("S:/Dropbox/Public/Rclick", recursive=T) # mit Dateien in Unterordnern
file.info(dir())         # Detaillierte �bersicht
file.show("Daten/3_Einlesen.txt" ) # eine Datei anzeigen
# Dateinamen sind, anders als Zuordnungen, nicht case-sensitive (GROSS/klein).


getwd()
setwd("..") # 1 Ordner aufw�rts. mit setwd("/") zur Partition, z.B. "D:/"
getwd()



# 3.2 Tabellen einlesen --------------------------------------------------------
MeiTab <- read.table("Daten/3_Einlesen.txt", header = T)
# Der Dateiname kann auch als ganzer Pfad angegeben werden, bei Weitergabe von
# Skripten und Daten sind aber relative Pfade ganz toll!
# read.table wichtigsten Argumente
# header = TRUE  �bernimmt Spalten-�berschriften
# dec = ","  liest Kommas als Dezimaltrennstellen
# sep = "-"  Nimmt Striche als Spaltentrennung an
# fill = T  f�llt L�cken in den Daten mit NA
#        (bei fill=F (Default!) m�ssen alle Zeilen und Spalten gleich lang sein)
# skip = 12  �berspringt die ersten 12 Zeilen (zB mit Metadaten)
# comment.char = "%" �berspringt Zeilen, die mit % anfangen (wie # in R)

# read.csv() f�r csv-tabellen und read.csv2() f�r deutsche Tabellen.
# Excel-Tabellen m�glichst als txt-Dateien exportieren ("Speichern unter")

?stackloss # W�rme-Abgasverlust - Effizienz industrieller Ammoniumoxidation

# Spaltennamen anzeigen
names(MeiTab)

# Grundlegende statistische Auswertung (Zusammenfassung) der Daten
summary(MeiTab)

# h�ufige Fehlermeldung beim Laden von Tabellen
# "Kann Datei ___ nicht �ffnen: No such file or directory"
# --> Verzeichnis oder Pfad kontrollieren: getwd()
# --> Dateiname kontrollieren
# --> Wird die Datei mit Anf�hrungsstrichen aufgerufen?  

# Wer viele Excel-Dateien einlesen muss: Ganz unten Abschnitt 3.7 lesen



# 3.3 Anzeigen ausgew�hlter Elemente -------------------------------------------
# (k�nnen durch Zuordnung auch als neue Tabelle/Vektor gespeichert werden)
MeiTab["Air.Flow"]              # Spalte "Air.Flow" als Spalte eines Dataframes
MeiTab[1]                       # Spalte 1 als Spalte                 
MeiTab$Air.Flow                 # Spalte "Air.Flow" als Vektor
MeiTab[,"Air.Flow"]             # Spalte "Air.Flow" als Vektor
MeiTab[,1]                      # Spalte 1 als Vektor
MeiTab[1:3]                     # Spalten 1 bis 3
MeiTab[3:7,2:3]                 # Teil der Tabelle
MeiTab[1,]                      # Zeile 1 als Tabelle
MeiTab[1:3,]                    # Zeilen 1 bis 3
MeiTab[ 6:12 , 2:3 ]            # Zeilen 6 bis 12, Spalten 2 und 3
MeiTab[1,"Air.Flow"]            # Element der Zeile 1 in der Spalte "Air.Flow"
MeiTab[ 1:3 , 4 ]               # Zeilen 1 bis 3 in Spalte 4 als Vektor
MeiTab[MeiTab$Air.Flow<60,]     # Tabellenzeilen mit Air.Flow < 50
MeiTab[MeiTab$Air.Flow>=70,]    # Tabellenzeilen mit Air.Flow gr�sser/gleich 70
MeiTab[MeiTab$Air.Flow==62,]    # Tabellenzeilen mit Air.Flow = 62 
MeiTab[MeiTab$Air.Flow!=62,]    # Tabellenzeilen mit Air.Flow ungleich 62
MeiTab$Water.Temp[MeiTab$Air.Flow>70]   # Water.Temp mit Air.Flow > 70

# Ausgaben als Spalte (Data.Frame) sind �bersichtlich
# Mit Vektoren kann aber oft bessser gerechnet werden
mode(MeiTab[1])  ;  mode(MeiTab[,1])

# h�ufige Fehlermeldung: Fehler in "__": undefined colums selected.
# --> beachte das Komma! Objekt[zeilen , spalten]
# --> dim(Objekt) zeigt dir, ob es �berhaupt soviele Spalten hat, 
                  # wie du ansprechen willst.



# 3.4 schlechtes Programmieren: attach -----------------------------------------
# Anh�ngen einer Tabelle
# mit attach() wird ein data.frame in den R Suchpfad eingebunden
# die einzelnen Spalten k�nnen jetzt mit ihrem Namen direkt angesprochen werden.
# Sind aber keine Objekte im normalen Environment (Vgl ls() vor und nach attach)
ls()
Air.Flow          # noch nicht bekannt
attach(MeiTab)
Air.Flow          # jetzt direkt abrufbar
ls()

log(Air.Flow)             # als Zeile  (Vektor, "numeric")
log(MeiTab["Air.Flow"])   # als Spalte (Tabelle, "list")
MeiTab[Water.Temp>22,] 

# attach ist generell eine Unsitte, weil man sp�ter nicht wei�, woher ein objekt
# kommt: aus dem normalen Workspace oder aus einer Dataframe.
# Und es kann Objekte und Funktionen im Base "�berschreiben".

search() # zeigt Suchpfad, wo die angeh�ngten Sachen sind

# Abh�ngen einer Tabelle (Die Tabelle selbst ist weiterhin im Workspace)
detach(MeiTab)



# 3.5 Tabelle exportieren (schreiben) ------------------------------------------
# Wenn kein Pfad angegeben, wird im working directory abgespeichert
write.table(tab1, "MeineErsteTabelle.txt", row.names=F, sep="\t", quote=F)
# row.names = FALSE : Abspeichern ohne Zeilennummern links
# col.names = FALSE : Spalten�berschriften nicht mitschreiben
# quote = FALSE     : Keine Anf�hrungsstriche in die Textdatei schreiben
# sep = "\t"        : Daten tabstopp-getrennt speichern (ab 7 Zeichen pro Spalte
#     ist die Darstellung im Editor verschoben). Zum Copypasten in Excel hinein.
# Escape-Sequenz "\" im Character belegt, Siehe Kap 17.1 
# Kann daher f�r Verzeichnisse nicht genutzt werden (siehe oben unter 3.1).



# 3.6 Oft verwendeter Operatoren (to be continued) -----------------------------
MeiTab
order(MeiTab$Water.Temp) # Reihenfolge
MeiTab[ order(MeiTab$Water.Temp) , ] # Tabelle sortiert nach Wassertemperatur
nrow(MeiTab)    # Anzahl Zeilen  (rows)
ncol(MeiTab)    # Anzahl Spalten (columns)
dim(MeiTab)     # entspricht c(nrow(),ncol()), macht das aber auch f�r arrays
t(MeiTab)       # transponieren: Zeilen zu Spalten, Spalten zu Zeilen

?split()        # Vektoren zerlegen
data()          # bestehende (R- oder Package-integrierte) Datens�tze anzeigen

# Und f�r Matrizenoperationen:
?det()          # Determinante einer Matrix
?solve(A,b)     # L�sung des Gleichungssystems A x = b
?crossprod(X,Y) # Kreuzprodukt zweier Matrizen
?qr()           # QR-Matrixzerlegung
?eigen()        # Eigenwerte und -Vektoren
?"%*%"          # Produkt zweier Matrizen  / Skalarprodukt zweier Vektoren


                              
# 3.7 Excel-Dateien lesen und schreiben ----------------------------------------

# http://cran.r-project.org/doc/manuals/R-data.html#Reading-Excel-spreadsheets
# F�r Excel 2007/2010/2013 siehe besonders das Paket xlsx
# Einzelne Dateien lassen sich schneller als Text- oder CSV-dateien speichern.

# Zun�chst Java installieren, falls nicht vorhanden:
browseURL("http://www.java.com/en/download/manual.jsp")
# ASK Toolbar beim Installieren ausschalten
# Beim Fehler JAVA_HOME cannot be determined from the Registry:
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version
library(rJava)

if(!require(xlsx)) install.packages("xlsx")
library(xlsx)
read.xlsx("filename.xlsx", sheetIndex=2)


