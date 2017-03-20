### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 10: if, for (Programmieren) ########################################

# 10.1 Bedingungen (conditions): if, ifelse

# Schleifen (loops): for, repeat, while
# 10.2 Allgemeines zu for-Schleifen
# 10.3 repeat- und while-Schleifen
# 10.4 Anwendungsbeispiel
# 10.5 Geschwindigkeit for-Schleifen



# 10.1 Bedingungen (conditions) ------------------------------------------------
# Syntax: if(Bedingung) {Ausdruck1} else {Ausdruck2}     bei 1 Wahrheitswert
# Syntax: ifelse(Bedingung, Ausdruck1, Ausdruck2)    für Vektor mit mehreren T/F
# Wenn Bedingung wahr, dann Auswerten von Ausdruck 1,
# wenn Bedingung falsch, dann Auswerten von Ausdruck 2.

7-3 > 2         # TRUE
class(7-3 > 2 ) # logical, Wahrheitswert
if(7-3 > 2) 18  # Bedingung ist TRUE, also gebe 18 zurück
if(7-3 > 5) 18  # Bedingung ist FALSE, also passiert nichts
if(7-3 > 5) 18 else 17 # Bed. F, also Rückgabe von 17

par(mfrow=c(2,1), cex=1)

if(TRUE) plot(1:10) else plot(10:1) # Bedingung True, also aufsteigende Punkte

# Mehrere Befehle müssen mit {} zusammengehalten werden:
if(FALSE) 
  {
  plot(1:10)
  box("figure", col=4, lwd=3) 
  } else 
    {
    plot(10:1) 
    box("figure", col=2, lwd=3, lty=2) 
    }
# Code einrücken macht ihn menschenlesbar!
# Führe den Block auch mal mit einer erfüllten Bedingung aus
# Tinn R: R Send: contiguous (echo=T) # Siehe Kap. 10.1 falls .trPaths fehlt.

# Unterschied zwischen "if(..) .. else .." und "ifelse(...)":
# if wenn die Bedingung nur einen Wert hat
# ifelse wenn ein ganzer Vektor mit Bedingungen vorliegt.
bart <- c(13, 14, 15, 16, 17)
bart>14
bart
ifelse(bart>14, bart+1, bart-10)
if(bart>14) bart+1 else bart-10 # Nimmt nur 1. Element, und warnt dabei. 

simpson <- -3:8
sqrt(simpson)  # Warnt vor Wurzel aus negativen Zahlen
sqrt(ifelse(simpson >= 0, simpson, NA))  # läuft
sqrt(if(simpson >= 0) {simpson} else NA)  # geht nicht, denn simpson ist Vektor.

?"if"
# Zeilenumbrüche zwischen '}' und 'else' sind zu vermeiden, wenn via Editor
# die Befehle Zeilenweise an R geschickt werden!

ergebnis <- 1/4 # Ergebnis vorheriger Berechnungen
if( ergebnis > 0.2 )  message("Ergebnis ist > 0.2 und das ist relevant.\n")
# Sowas kann man gut in Funktionen einbauen!
# Eine ganze Reihe von Bedingungen ist daher in Kapitel 12 verwendet.

# Zur Abkürzung T und F siehe Kapitel 19.5 Logische Operatoren

# Wenn Rechenzeit für dich wichtig wird: 
# "ifelse() is for vectorized conditions. if(){} else{} is more efficient 
# for length 1 conditions than ifelse()"  Uwe Ligges, R-help 2004



# Schleifen (loops) ------------------------------------------------------------
# 10.2 Allgemeines zu for-Schleifen --------------------------------------------
# mehrfaches Ausführen eines Codeblocks, zB Erstellen mehrerer Bilder
# Syntax: for ( EinLaufbuchstabe in EineSequenz) { machwas }
# Zwischen den {} können mehrere Befehle stehen, die i-fach / für i verschiedene
# Werte durchgeführt werden, auch über mehrere Zeilen getrennt.
# Bei einem einzelnen Befehl können die {} weggelassen werden.
help("for")          
for(i in c(2,5,9) ) print(1:i) # man verwendet oft i für index
# im ersten Durchlauf wird an den Befehl i mit dem Wert 2 übergeben.
# Der Befehl wird ausgeführt, dann fängt die Schleife von vorne an und führt
# den Code mit dem nächsten Wert aus.


# mit for einen Vektor füllen
v <- vector("numeric", 20)
v
for(i in 3:17) v[i]= (i-2)^2 # für jedes i wird der Code einmal ausgeführt
v
# Ist in R sehr langsam - vektorisieren ist 100 fach schneller. -> 10.2.4


# Erstellen mehrerer Graphiken. Besonders sinnvoll in Kombination mit anderen
# Devices, zB png, bmp oder pdf (Siehe Kap 4.10)
windows(record=T) # Aufzeichnung der Einzelbilder starten
plot(1, ylim=c(0,7))
plot(3, ylim=c(0,7))
for (i in 1:7)  plot(i, ylim=c(0,7)) # mit Bild Up, Bild Dn durchklicken

# Addieren mehrerer Linien zum Plot
z3 <- seq(0,25,0.1)
plot(z3,z3, type="n")
lines(z3, 12.5*5/factorial(4)*(z3/8*5)^(4)*exp(-z3/8*5), col=2 )
lines(z3, 12.5*6/factorial(5)*(z3/8*6)^(5)*exp(-z3/8*6), col=2 )
for (n in 7:25)
lines(z3, 12.5*n/factorial(n-1)*(z3/8*n)^(n-1)*exp(-z3/8*n), col=2 )


# Laufende Dateinamen oder Objektnamen erstellen:
getwd() # mit setwd auf einen Ordner mit Schreibberechtigung setzen
# Ja, beschränkte Rechte auf C:\ sind für viele (unerfahrene) Nutzer sinnvoll!
meinevar <- c(12,14,15,28,76)
for(k in 1:4) { out <- rexp(k)
   write.table(out, paste("MeinName", meinevar[k], ".txt", sep="") , quote=F)}
cat("Hier sind deine Textdateien:\n", getwd(), "\n")  # \n newline, Kap 17.1
# Jetzt Dateien wieder löschen
unlink(paste("MeinName", meinevar[1:4], ".txt", sep="") ) # riskanter Befehl!
# Das Skript kann sich zB selbst löschen, wenn man den ganzen Ordner löscht!



# 10.3 repeat- und while-Schleifen ---------------------------------------------

# Wiederholung von Befehlsaufrufen, Iterationen, Simulationen
# Syntax: repeat {Ausdruck} # Wiederholung des Ausdrucks
#                next # Sprung zum nächsten Iterationsschritt
#                break # Verlassen der Schleife
# Syntax: while(Bedingung){Ausdruck} # Wiederholung, solange Bed. erfüllt
i <- 0 # Lege Objekt i mit dem Wert 0 an
repeat # wiederhole nachfolgenden Codeblock immer wieder
   {                    # Codeblock beginnt
   i <- i + 1           # überschreibe Objekt i mit neuer Zahl
   if (i < 3)  print(i) # wenn i kleiner ist als 3, dann Ausgabe in der Konsole
   if (i == 3)   break  # wenn i gleich 3 ist, wird die Schleife abgebrochen.
   }                    # Codeblock endet

i # beim letzten Durchlauf wurde 1 zu 2 hinzugefügt (=3), und das als i angelegt
  
i <- 0
while(i < 3) i <- i + 1 # Während (while) 1<3 ist, wird Code ausgeführt
i

# Werden beides in der Umwelt-Datenanalyse sehr selten angewendet, repeat habe
# ich sogar noch nie in der Realität gebraucht.



# 10.4 Anwendungsbeispiel ------------------------------------------------------
# Punkte zufällig festlegen, aber mit Mindestdistanz zueinander.
set.seed(42) # Startpunkt für den Zufallszahlengenerator festlegen. So hast du
# die gleichen Ergebnisse wie ich.
# Zielvektor für 20 zufällig verteilte Punkte erstellen:
x <- rep(NA, 20)
# Ersten Wert reinschreiben:
x[1] <- runif(1, 10, 80) # Zufallszahlen gleichmäßig zwischen 10 und 80 verteilt
x
count <- 1 # Anzahl Versuche, die benötigt werden
# 19  Punkte hinzufügen
for(i in 2:20)                   # Code zur Übersichtlichkeit einrücken!
   {
   x[i] <- runif(1, 10, 80) # random aus uniform-verteilung
   count <- count+1
   # Wenn eine minimale Distanz von 1.7 zu den anderen Punkten nicht gegeben 
   # ist, wird der Punkt ersetzt, also nochmal zufällig gezogen
   while( min( abs(x[i] - x[-i]), na.rm=T ) < 1.7 ) 
        {
        x[i] <- runif(1, 10, 80)
        count <- count+1
        } # Ende while-Schleife  |  schreibe ich bei mehreren } oft dazu, damit
   } # Ende for-Schleife         |  ich schnell sehen kann, wozu es gehört.
x
count # 45 versuche, 20 Punkte zwischen 10 und 80 mit dist > 1.7 zu verteilen.

plot(x, rep(1,20))
# Jetzt sind die Punkte zufällig gesetzt, aber immer mit einer minimalen Distanz
# zueinander. Dies ist für Versuchsplanungen (sampling design) interessant.
# Um Methoden der statistischen Inferenz (Schluss von einer Stichprobe auf die 
# Grundgesamtheit) anwenden zu können, müssen die Stichpr. zufällig gezogen sein
points(runif(80, 10, 80), rep(c(0.6, 0.8, 1.2, 1.4),each=20), col=2, pch=17)
# Liegen mitunter sehr geklumpt und decken weniger gut den ganzen Bereich ab.



# 10.5 Geschwindigkeit for-Schleifen -------------------------------------------
# Generell sind Schleifen in R in der Berechnung um ein Vielfaches langsamer als
# vektorisierte Rechnungen. Bei größeren Datensätzen (> 500 Zeilen zB) solltest
# du sie nach Möglichkeit vermeiden und lieber apply (Abschnitt 10.3) verwenden.
# Sind sie trotzdem nötig, macht es immer Sinn, erst den gesamten Vektor mit
# NA oder 0 Werten zu erstellen, und diese dann einzeln zu füllen. Denn sonst,
# wenn ein leerer Vektor mit sovielen Elementen gefüllt wird, muss er nach jedem
# Schleifendurchlauf neu gespeichert werden --> "cannot allocate"-Fehler.

# Beweis: Vektorisieren ist besser!
x <- rnorm(5e6) # 5 Mio Zufallszahlen aus der Standardnormalverteilung
system.time(  for (i in seq(along=x)) x[i] <- x[i]^2  ) # 11.42 Sekunden bei mir
system.time(  x <- x^2 ) # vektorisierte Alternative    #  0.03 Sekunden!

# Beweis: Vektor mit richtiger Länge vorbereiten ist besser!
x <- rnorm(1e5)
y <- numeric() # Leerer Vektor der Länge Null
system.time(  for (i in seq(along=x)) y[i] <- x[i]^2  ) # 12.58 Sek
y <- numeric(1e5) # Leerer Vektor mit 0.1 Mio Elementen
system.time(  for (i in seq(along=x)) y[i] <- x[i]^2  ) #  0.22 Sek
system.time(  y <- x^2  )                               #  0    Sek

# Merke: Schleifen sind langsam in R! (in z.B. FORTRAN oder C nicht)
# - Vermeide lange Schleifen durch Vektorisierung
# - für Informatiker und Nerds: Code-Auslagerung "Calling C and Fortran from R":
browseURL("http://cran.r-project.org/package=Rcpp")
browseURL("http://users.stat.umn.edu/~geyer/rc/")

# - Oder lerne, apply Funktionen anzuwenden. Die sind schnell und oft ganz toll,
# aber leider für Anfänger nicht immer leicht zu lernen.
# Da sich das aber wirklicht lohnt, soll das nächste Kapitel (11) dabei helfen.
