### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel V (Verschiedenes): Rprofile und weitere Tipps ######################

# V.1 Rprofile.site
# V.2 Verschiedene Tipps
# V.3 Workspace speichern
# V.4 Pakete  # ToDo: Info aus dem gesamten Handbuch hier b�ndeln
# V.5 Logische Operatoren
# V.6 Gleichheit Kommazahlen
# V.7 Fehlermeldungen
# ToDo: Sammlung Fehler und L�sungen (vielleicht in ein fr�heres Kapitel?)


# V.1 Rprofile.site ------------------------------------------------------------
# TinnR kann mehrere Zeilen Code (bis zur n�chsten Leerzeile) mit einem Klick
# an R senden (R send: contiguous), braucht aber besondere Pfade dazu:
.trPaths <- paste(paste(Sys.getenv('APPDATA'), '\\Tinn-R\\tmp\\', sep=''),
 c('', 'search.txt', 'objects.txt', 'file.r', 'selection.r',
 'block.r','lines.r'),sep='')
# Praktisch ist es, die Zuordnung dauerhaft an R zu �bergeben. Kopiere ihn dazu
# in "Rprofile.site" (im R-Programmordner (wo R installiert ist) unter "etc").
# Die Datei kann mit Editor oder �quivalenten ge�ffnet werden.
# Nach dem n�chsten R Start k�nnen mehrere Zeilen auf einmal gesendet werden.
# Siehe auch RclickHandbuch.wordpress.com/install-r/rprofile

# Auch eigene Zuordnungen, zB Funktionen, lassen sich im Rprofile
# speichern, und m�ssen dann nicht mehr jedes Mal an R geschickt werden.
# Das alles geht leider nur, wenn Admin-Rechte vorliegen.
# Falls nicht: erstelle ein Skript mit allen Zuweisungen mit einem sinnvollen 
# Namen (zB Funktionen.r), und speichere das auf der Festplatte. Dann muss nur
# am Anfang jeder Sitzung einmal source("D:/Funktionen.r") ausgef�hrt werden.
?Rprofile
# Wichtig: Sachen in Rprofile �berschreiben ls(.BaseNamespaceEnv)!
# Schreibe nichts da rein, was es im base von sich aus gibt!

# Praktisch: Dateipfade in Rprofile speichern
desktop <- ("C:/Dokumente und Einstellungen/berry/Desktop")
# Danach in jedem Skript m�glich:
setwd(desktop)

# Setze den CRAN mirror (Wo Pakete runtergeladen werden)
local({r <- getOption("repos")
      r["CRAN"] <- "http://mirrors.softliste.de/cran" # Berlin
      options(repos=r)})

# leute richtig verwirren (heimlich in deren Rprofile schreiben):
pi <- 3
library(fortunes); fortune(352)
# oder richtig dissen:
q("no") # Bitte nicht ausprobieren, wenn grad viele Objekte zugeordnet sind...


# Graphiken standardm��ig mit den y-Achsenwerten aufrecht erstellen
setHook("plot.new", function(...) par(las=1), "append")
# Geht aber noch nicht. Ideen vor!
# Note to self: 
?windows.options # Examples

# Mehr unter RclickHandbuch.wordpress.com/install-r/rprofile



# V.2 Verschiedene Tipps -------------------------------------------------------
# Viele Tipps zum angenehmen Programmieren via TinnR finden sich hier:
# http://rclickhandbuch.wordpress.com/install-r/tinn-r/

# Lange Berechnungen in der R-Konsole k�nnen mit ESC abgebrochen werden.
# Diese sind daran sichtbar, das noch kein ">" in der letzten Zeile erscheint.

round(17.8)
as.integer(17.8)  # h�ttst jetzt 18 erwartet, wa?
#
x <- -1:12         # %/%: ganze Teiler;  %%: Rest (modulo operation)
data.frame(x, Teiler_5=x%/%5, Rest=x%%5 )


# Spaltennamen, die mit einer Zahl beginnen, wird ein X vorgesetzt
data.frame("2.7.2010"=12:18, ZweiteSpalte=18:12)
# Das passiert auch bei mit read.table geladenen Dataframes!
# Es kann aber ersetzt werden (praktisch zB f�r Subs von Boxplots):
# names(Objekt) <- c("1.Tag", "2. Messung") # sogar Leerzeichen sind OK!


# sourcecode von Funktionen:
lm
summary
methods(summary)
summary.table


# Nachkommastellen (der Ausgabe in der RConsole) einstellen
d = data.frame( a=rnorm(10) , b=rnorm(10) )
options(digits=4) # �bersichtlichkeit der Datendarstellung einstellen
d   
print(d[1,1], digits=18) # Die Daten selbst haben noch viele Nachkommastellen.
options(digits=7) # Standardeinstellung



# Umgang mit NAs (Not available, unbekannt)
v <- c(7,9,NA,5,6)
cumsum(v)   # 7 16 NA NA NA
# Inhaltlich falsch, aber wenn man davon ausgeht, dass NAs 0 entsprechen:
# Ersetze in v alle Werte == NA mit 0 (replace)
cumsum( replace(v, is.na(v), 0) ) # 7 16 16 21 27
# Alternative: Mittelwerte der nicht-NA-Werte:
cumsum(replace(v,is.na(v),mean(v, na.rm=T))) #  7.00 16.00 22.75 27.75 33.75



# V.3 Workspace speichern ------------------------------------------------------
# Beim Schlie�en von R braucht das WorkSpace normalerweise nicht gespeichert zu 
# werden, es kann ja mittels des Skripts in TinnR erneut hergestellt werden.
# Sp�testens nach vierst�ndigen Berechnungen sollte es aber gespeichert werden.
# siehe dazu
save(list="Objekt", file="Dateiname.RData")
?save.image # F�r das gesamte Workspace, also kurz f�r save(list=ls(), ".RData")
# mit rm("Objekt7") kann man Objekte einzeln l�schen und dann save.image nutzen.
load("ewaigeSubordner/Dateiname.RData")
?history
# Nach solchen gro�en Berechnungen kann es sinnoll sein, R zu schlie�en und neu 
# zu �ffnen. Siehe dazu die relative Gr��e der Auslagerungsdatei im Taskmanager 
# (STRG + ALT + ENTF),  (unter Linux in der System�berwachung)



# V.4 Pakete -------------------------------------------------------------------
library() # zeigt alle runtergeladenen und mitinstallierten packages
library("datasets")
library(help="datasets")
# Etwas zu Paketen, Vgl auch Kapitel 4.12
library(alr3) # geht noch nicht
install.packages("alr3") # nur einmal abschicken! Nicht in RProfile stecken!
# jetzt wurde das Paket gedownloaded und installiert.
# Aus Recheneffizenzgr�nden musss es aber nach jedem R-neustart geladen werden.
library(alr3) 
# in eigenen Funktionen sollte require statt library benutzt werden.
# Dann werden Fehlermeldungen a la "Wurde unter R 2.13 erstellt" unterdr�ckt.



# V.5 Logische Operatoren ------------------------------------------------------
2>4 
!2>4 #  NICHT
A <- c(TRUE, TRUE, FALSE, FALSE)  ; B <- c(TRUE, FALSE, TRUE, FALSE)
?"&"
data.frame(A, B,    Nicht_A = !A,    A_und_B = A&B,    A_oder_B = A|B)

dat <- rnorm(300)  ; hist(dat)
dev.off()
hist(dat, plot=F)
F 
F <- 3
F
hist(dat, plot=F)
# F geht nur als Abk�rzung f�r FALSE, solange es nicht �berschrieben wird. 
rm(F)
F

# Viele Anwendungen von TRUE/FALSE auch in Bedingungen --> Kap 11.1: if

dat <- sample(-6:10)
dat > 4
which(dat > 4) # An welchen Stellen eines logical Vectors steht TRUE? 
sum(dat > 4) # Anzahl TRUE. Funktioniert, da TRUE = 1 und FALSE = 0
dat < -2 # Leerzeichen wichtig! dat <-2 w�rde "dat" mit der Zahl 2 �berschreiben
dat<(-2) # ist schlecht menschenlesbar, aber eindeutig.



# V.6 Gleichheit Kommazahlen ---------------------------------------------------
0.5-0.2 == 0.3 # ja nee, is klar. Denkst du?!  Dann check mal das:
0.4-0.1 == 0.3 # Grundgedanke aus "The R-Inferno" entnommen - sehr lesenswert!
# Zweiteres zeigt FALSE, obwohl es richtig ist! Folgendes ist der Grund:
# ein Rechner kann Flie�kommazahlen nur endlich genau verarbeiten - arbeitet
# mit bin�ren Zahlen - abh�ngig von bit-Betriebssystem, Prozessor-Architektur, 
# definition des Zahlenformats (single = 1 byte = 8bit, double = 2 byte), usw.
# Am Rande f�r Informatiker: R verwendet nur Double, siehe Note zu ?numeric.
print(0.4, digits=20)
print(0.1, digits=20)    # Schlagwort: arithmetische Pr�zission
print(0.3, digits=20)
print(0.4-0.1 , digits=20)

# Folgendes ist die L�sung f�r einzelne Zahlen:
all.equal( 0.4 - 0.1  , 0.3 ) # TRUE (l�sst PC-Ungenauigkeit zu) 

# Aber nicht f�r ganze Vektoren:
set.seed(12); zahlen <- sample( 1:8+3:6/10, 30, replace=T) ; zahlen
all.equal(zahlen, 4.6) # geht nicht
# Wenn man z.B. das Ergebnis einer anderen Rechnung verwenden will:
zahlen == 4.9-0.3 # Alles FALSEs, obwohl es 4.6 gibt!
4.9-0.3 == 4.6 # Absolute Gleichheit nicht gegeben

# L�sung f�r Zahlenvergleich von Vektoren:

# Komplizierte (aber allgemeing�ltige) L�sung:
TF_Werte_einzeln <- by(zahlen, 1:length(zahlen), FUN=all.equal, current=4.9-0.3)
welcheT <- which(TF_Werte_einzeln==T); welcheT 
str(TF_Werte_einzeln)
str(welcheT) # welcheT hat names, aber das st�rt ja nicht
names(welcheT) <- NULL   ; welcheT # Falls es doch mal st�rt ;-) 

# einfache L�sung:
which( zahlen == round(4.9-0.3, 8)  ) # Problematisch, wenn mehr
# Nachkommastellen wichtig sind, und man das nicht bemerkt!
# Randnotiz: Runden auf signifikante Stellen, also Zahlen nach Nullen:
round( 0.00314869, digits=3)
signif(0.00314869, digits=3)

# ANzahl notwendiger digits beim Ausgangsbeispiel:
round(0.4-0.1, 20) == round(0.3, 20)  # noch FALSE
round(0.4-0.1, 15) == round(0.3, 15)  # TRUE

# Merke: niemals Flie�kommazahlen einfach so auf absolute Gleichheit pr�fen!



# V.7 Fehlermeldungen ----------------------------------------------------------
# Unterschied Fehler und Warnung:
# Fehlermeldungen treten bei Fehlern auf (Ach nee). --> Abbruch der Funktion.
# Warnmeldungen sollten gelesen werden, weil die Funktion nicht abgebrochen,
# sondern wahrscheinlich anders ausgef�hrt wird, als du willst!


# h�ufige Fehlermeldungen:
# "Unerwartete(s) Symbol"
# Meistens eine Klammer noch nicht wieder geschlossen, ein Komma vergessen, etc.

# Sammlung to be ausgedehnt.
