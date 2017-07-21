### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 11: apply-Funktionen ###############################################

# Motiviert durch niedrige Geschwindigkeit for-Schleifen (Kapitel 10.5)
# apply und Co sind außerdem eleganter!

# 11.1 die Anwende(apply)-Funktionen in Übersicht
# 11.2 apply: "anwenden"
# 11.3 tapply: tagged-apply (pr Kategorie)
# 11.4 lapply: list-apply   und   sapply: simplify-apply
# 11.5 mapply: multiple-apply

# 11.6 Rechenzeit for, lapply, apply
# 11.7 Parallel Computing unter Windows
# 11.8 Übungsaufgabe



# 11.1 die Anwende(apply)-Funktionen in Übersicht ---Argumente----------------
?apply # Spalten- oder zeilenweise Funktion ausführen  X, MARGIN, FUN
?tapply; ?by # table/tag-apply in Gruppen (Tags)       X, INDEX, FUN, simplify
?aggregate # tapply für data.frames statt nur Vektoren x, by, FUN, simplify
?lapply # list-apply                                   X, FUN
?sapply # simplify-apply, sonst wie lapply             X, FUN, simplify
?mapply # multiple-apply                               FUN, MoreArgs, SIMPLIFY
# Leider sind die Argumente recht unterschiedlich benannt und geschrieben...
# Daher lasse ich sie oft unbenannt. In der Default-reihenfolge geht das ja.
# Eine sehr stark verkürzte Variante dieses Kapitels (auf englisch):
browseURL("http://stackoverflow.com/a/7141669/181638")
# Auch diese Seite finde ich ziemlich gut:
browseURL(
"http://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r")



# 11.2 apply: "anwenden" -----------------------------------------------------
m <- matrix(sample(1:100, 24), ncol=6)
rownames(m) <- letters[1:4] ; colnames(m) <- LETTERS[21:26]
m

apply(X=m, MARGIN=2, FUN=mean) # mean spaltenweise (MARGIN=2) anwenden
apply(X=m, MARGIN=1, FUN=max) # max zeilenweise (MARGIN=1) anwenden  --> vector
apply(X=m, MARGIN=1, FUN=range)                                    # --> matrix
colMeans(m) # für einfache Funktionen gibt's die auch direkt
rowSums(m)

# Übung: Erstelle eine Tabelle mit mu und sd von m, mit Zeilen- und Spaltennamen


# Anwendungsbeispiel apply: Array
# Wirtschaftsdaten mehrerer Länder, Jahre, und Sektoren
load("Daten/11_econ.Rdata") # fiktive Daten aus Dominik Reusser's R-Kurs 2011
# Siehe Kap. 10.3
ls()
econ
str(econ)
class(econ) # array, also matrix mit (hier) 3 Dimensionen - gut für regelmäßige
mode(econ) # numeric, also alles mit Zahlen                 Zeitreihen geeignet
# Indizierung von Arrays
dimnames(econ)
econ["DE", ,]         # Daten von Deutschland anzeigen
econ[,"Industrie",]   # Industrie-Daten
econ[,,"2003"]        # Daten aus 2003, Analog:
econ[,,2] # zweites Element der dritten Dimension (hier: "2002")

# Mittelwerte über Länder, Jahre, und Sektoren
# Das MARGIN-Argument gibt an, welche Dimension behalten wird:
apply(econ, MARGIN=c(1, 2), FUN=mean)
apply(econ, 2:3, mean)      # bei Angabe in richtiger Reihenfolge können 
apply(econ, 1, mean)        # Argumentnamen weggelassen werden 
apply(econ, 2, mean)
apply(econ, 3, mean)
apply(econ, 3, quantile)

# Siehe auch:
browseURL("http://de.wikibooks.org/wiki/GNU_R:_apply")



# 11.3 tapply: tagged-apply ---------------------------------------------------
data() # Zur Verfügung stehende Datensätze
head(chickwts, 15) # Hühnerwachstum abhängig von Fütterungstyp
tapply(X=chickwts$weight, INDEX=chickwts$feed, FUN=mean)
# Der Datensatz X wird anhand des INDEX gruppiert, bevor auf jede Gruppe die
# Funktion FUN angewendet wird. Die Ausgabe ist vom Typ array, mit Namen.
# t-apply steht ggf für tagged apply (the tags identify the subsets)
sort(tapply(chickwts$weight, chickwts$feed, mean))
# Für histogramme pro Gruppe siehe # 11.5 mapply

# Übung: schaue via tapply nicht nur mean, sondern auch min und max an


# Eine Funktion, die je zwei Werte zurück gibt, wird als array ausgegeben:
tapply(chickwts$weight, chickwts$feed, range)
futtergew <- tapply(chickwts$weight, chickwts$feed, range)
View(futtergew) # Objekts wird dabei intern in ein data.frame umgewandelt
sapply(futtergew, I) # macht ein echtes Dataframe draus
# sapply ist in 11.4 und 11.5 erklärt und wendet hier die Funktion I auf
# jedes Element der Liste an. I gibt im Wesentlichen den jeweiligen Input ohne
# Attribute zurück.

class(tapply(chickwts$weight, chickwts$feed, mean))
class(tapply(chickwts$weight, chickwts$feed, range))


# Anwendungsbeispiel tapply: iris-Daten 
head(iris) # Größen von Kelch- und Kronenblätter dreier Schwertlilienarten
?iris
str(iris)
tapply(X=iris$Petal.Length, INDEX=iris$Species, FUN=mean)
tapply(iris$Petal.Length, iris$Species, max)
summary(iris)
tapply(iris$Petal.Length, iris$Species, summary)


# Für Tabellen verwendet man "by", annstatt INDEX heißt das Argument da INDICES
by(iris[,1:4], INDICES=iris$Species, FUN=sum)


# mit mean funktioniert das nicht so wie bei Neil Saunders.
# ToDo: rausfinden, warum nicht.

# Für ganze Datensätze aggregate verwenden (tapply nimmt als X nur einen Vektor)
state.x77 # Info über USA-Staaten
state.region # Regionen der Staaten der USA
cbind(state.x77, state.region) # schön sichtbar, dass die Regionen factors sind,

# die intern mit ganzen Zahlen beschrieben werden
aggregate(state.x77, by=list(Region = state.region), FUN=mean)



# Datensätze nach Kriterium auftrennen:
tapply(chickwts$weight, chickwts$feed, function(x) x) # Funktion gibt input
# geht eleganter mit                                  # unverändert zurück
tapply(chickwts$weight, chickwts$feed, I)
# und noch eleganter mit
split(chickwts$weight, chickwts$feed)


# Anwendungsbeispiel tapply: Gruppierung nach mehreren Faktoren
head(warpbreaks)
str(warpbreaks) 
# mittlere Anzahl Garn-Brüche (X) beim Weben nach Fadenspannung (INDEX):
tapply(X=warpbreaks$breaks, INDEX=warpbreaks$tension, mean) 
# mittlere Anzahl Brüche nach Wolle-typ:
tapply(warpbreaks$breaks, warpbreaks$wool, mean)
# mittlere Anzahl Brüche nach Spannung und Wolle-typ:
tapply(warpbreaks$breaks, warpbreaks[,2:3], mean)


# Anwendungsbeispiel tapply: "gleitendes Mittel" (moving average)
# 10'000 äquidistante Daten. Mittelwert alle 100 Punkte. 
x <- cumsum(rnorm(10000)) # hypothetische Daten
plot(x, type="l", col=8)
mx <- tapply(x, rep(1:100, each=100), mean)
lines(seq(0,10000, leng=100), mx, col=2, lwd=2)
wid <- 100 # Breite des gleitenden Fensters fexibler
tapply(x, rep(1:(length(x)/wid), each=wid), mean)
# Andere Lösung:
rowMeans(matrix(x, ncol=100, byrow=TRUE))

# Ich bin zoo-Gegner, ich sehe den Nutzen nicht. Überzeuge mich doch :-)
install.packages("zoo")
require(zoo)
?rollapply # from package zoo
z2 <- zoo(round(rnorm(15),1)); z2
rollapply(z2, 3, mean, by = 3) # means of nonoverlapping groups of 3
# PS: ein echtes gleitendes Fenster hätte Überlappungen
# Hab deswegen eine rollapply Äquivalente für normale Daten in Kap 12 (movAv)



# 11.4 lapply: list-apply   und   sapply: simplify-apply ---------------------
bart <- list(1:10, 2:30, 3:40)
bart
lapply(bart, mean)    # list
sapply(bart, mean)    # vector (s für simplify list to vector)


# lapply statt for-Schleifen: Spalten gegeneinander plotten
dat.a <- as.data.frame(matrix(runif(20),ncol=4))
dat.a
dat.b <- as.data.frame(matrix(runif(20),ncol=4))
par(mfrow=c(2,2))
for(i in 1:4)                    plot(dat.a[,i], dat.b[,i], pch=16, main=i)
# lange for-Schleifen ersetzen mit lapply (rechnet um ein vielfaches schneller):
dummy <- lapply(1:4, function(i){plot(dat.a[,i], dat.b[,i], pch=16, main=i)} )
dummy # Hier nicht benötigter Kram, daher auch dummy genannt
graphics.off()


# Rechenzeit:
dat <- matrix(rnorm(5e6), ncol=50)
dim(dat) # 100'000 Zeilen, 50 Spalten
Erg <- NA # Vektor mit Ergebnis je Zeile: Wert genau zwischen min und max
system.time(    for(i in 1:100000)   Erg[i] <- mean(range(dat[i,]))    )
rm(Erg) # 53 Sekunden (Es lebe SSD! Bei alten PCs mehr)
system.time( Erg <- sapply(1:1e5, function(i) mean(range(dat[i,])) )  )
# 3.7 Sekunden = 7% der Rechenzeit der Schleife !!
hist(Erg)
# Man kann also (nichtrekursive) Schleifen ganz einfach schneller machen:
# Einfach "sapply(1:n, function(i)" und ")" drumherum schreiben.
# Und man muss vorher keine leeren Objekte erstellen.
Erg <- rep(NA, 100000) # auch 3.7 Sekunden
system.time(    for(i in 1:100000)   Erg[i] <- mean(range(dat[i,]))    )


# weiteres Beispiel - lapply und sapply im Vergleich:
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
x
dim(x); class(x) # list mit 3 elementen
length(x) # jedes dieser Bestandteile hat selbst wieder eigene Elemente
lapply(x, mean)
sapply(x, mean)   # simplify list to vector


# Anwendungsbeispiel sapply: Quantile
set.seed(2) # legt einen Startpunkt für den Zufallszahlengenerator fest, dadurch
# sind die reproduzierbar: du kriegst die gleichen "Zufalls"zahlen wie ich.
Ksat <- round(rexp(n=45, rate=0.02),1) # Random EXPonentially distributed
quantile(Ksat)
# 25% der Daten ist kleiner als 16.3
# 100%-75% = 25% der Daten ist größer als 73.7 (75% Quantil)
# 9-Zahlenmaß: Median, Quartile (Fourth), Oktile (Eighth), Hexadezile, Range
# Quantile kann verschiedene Algorithmen für die Berechnung verwenden (type)
# Um das schneller zu vergleichen, schreiben wir eine Funktion:
myquant <- function(tp) quantile(Ksat,prob=c(0,1,2,4,8,12,14,15,16)/16, type=tp)
# Die führt dann jeweils abhängig von tp quantile von Ksat aus
myquant(tp=1)   # Mehr zu Fuktionen in Kap 12. Wenn du nachvollziehen kannst,
myquant(3)      # was hier passiert, reicht das aber erstmal.
# insbesondere bei den hohen Quantilen ist das Ergebnis abhängig vom type!
# (weil dort in diesem Datensatz weniger Werte sind)
# Vergleich in einer Tabelle:
quants <- sapply(X=1:9, FUN=myquant); quants # simplify-apply
# deutlich eleganter (und schneller) als mit einer for-Schleife.
boxplot(t(quants), at=c(0:2,4,8,12,14:16)/16, boxwex=0.1, xlim=0:1,
        las=1, horizontal=TRUE)
# Methoden unterscheiden sich stark wenn wenig Werte da sind...
lines(ecdf(Ksat), verticals=T, do.points=F, col=2, lwd=3)


# Anderes Beispiel für Ökonomen:
# Fake Data: Einkommen von Leuten in verschiedenen Einkommensklassen
set.seed(42) # Startpunkt für Zufallszahlengenerator (zur Reproduzierbarkeit)
Einkommen <- round(c(rexp(20)*1e4, rexp(20)*1e5, rexp(20)*1e6))
Klasse <- rep(c("1.low", "2.mid", "3.high"), each=20)
# Funktion definieren, die die benötigten Kennwerte rausgibt:
dezile <- function(input) quantile(input, probs=c(0.1, 0.9) )
dezile(Einkommen) # funktioniert schonmal
# Jetzt nach Klassen getrennt:
tapply(Einkommen, INDEX=Klasse, FUN=dezile) # list Ausgabe unübersichtlich
sapply(X=tapply(Einkommen, INDEX=Klasse, FUN=dezile), FUN=I)


# lapply kann eine "list" zu einem "data.frame" umwandeln, siehe l2df im
# Paket BerryFunctions
install.packages("berryFunctions"); library("berryFunctions")
?l2df

# Ähnlich wie lapply funktioniert auch do.call:
eg3 <- list(KA=data.frame(SW=1:6, SB=2:-3), LS=data.frame(SW=23:24, SB=c(-3,2)))
eg3
do.call(rbind, eg3)



# 11.5 mapply: multiple-apply ------------------------------------------------
check.math <- function(a,b,c)
       cat(paste(a,"+",b,"=",c, ifelse(a+b==c,"ist richtig","ist falsch"),"\n"))
check.math(3,4,7)
check.math(4,5,7)

alang <- c(6,2,4,5)
blang <- c(5,3,5,9)
clang <- c(3,5,8,7)
dummy <- mapply(check.math, alang, blang, clang)

# Histogramme einzelner Gruppen mit Überschrift
par(mfrow=c(2,3))
mapply(hist, split(chickwts$weight, chickwts$feed), main=levels(chickwts$feed),
       MoreArgs=list(xlim=range(chickwts$weight), col=2, breaks=1:10*50))



# Für ProgressBars in *apply-Funktionen siehe:
#browseURL(paste("http://ryouready.wordpress.com/2010/01/11/",
#            "progress-bars-in-r-part-ii-a-wrapper-for-apply-functions", sep=""))
install.packages("pbapply")
library("pbapply")
dummy <- pbsapply(rep(0.02, 100), Sys.sleep)



# 11.6 Rechenzeit for, lapply, apply -------------------------------------------
n <- 1e3
dat <- matrix(rnorm(n*50), ncol=50)
library("microbenchmark")

mr_bad_for <- function() {Result <- NA
                          for(i in 1:n)  Result[i] <- mean(range(dat[i,])) ;      Result}
mr_good_for <- function() {Result <- rep(NA, n)
                           for(i in 1:n)  Result[i] <- mean(range(dat[i,])) ;      Result}
mr_sapply <- function() {Result <- sapply(1:n, function(i) mean(range(dat[i,])) );  Result}
mr_apply <- function() {Result <- apply(dat, MARGIN=1, function(x) mean(range(x))); Result}

system.time(mr_good_for())
system.time(mr_good_for())
# will be slightly different each time

# Folgende Berechnung dauert relativ lange! - n ggf. runtersetzen
speed <- microbenchmark(mr_bad_for(), mr_good_for(), mr_sapply(), mr_apply(), times=20)
speed
plot(speed) # if there is one extreme outlier, run it again

par(mar=c(3,7,2,1))
plot(speed, horizontal=TRUE, las=1)

library("ggplot2")
autoplot(speed)


# microbenchmark for other tasks:
library("microbenchmark")
a <- sample(0:1, size=3e6, replace=TRUE)
speed <- microbenchmark(a != 0, ! a == 0, times=50)
boxplot(speed, notch=TRUE, unit="ms", log=F)



# 11.7 Parallel Computing unter Windows ----------------------------------------
# Motivation: kürzere Rechenzeit   ein Literaturbeispiel von Tausenden:
browseURL("www.fernuni-hagen.de/FACHSCHINF/1727/Zusammenfassung.pdf")
# Für UNIX-Systeme gibt es die Pakete parallel und multicore - Für Windows snow
if(!require(snow)) {install.packages("snow") ; require(snow) }
cl <- makeCluster(4) # 4 Kerne auf dem Rechner

# Ggf. vorher notwendig: Objekte an Cluster übergeben:
clusterExport(cl, c("centroide","patchsize","distance"))
browseURL("http://stackoverflow.com/questions/10095956")

# Nimm eine kleinere Zahl, falls der PC für dieses Beispiel zu langsam ist:
sinnlos <- function(i) quantile(rnorm(1e8))
system.time( sinnlos(1) ) # 12 sek
system.time(       sapply(1:10, sinnlos) )  # 131 sek
system.time(parSapply(cl, 1:10, sinnlos) )  # 42 sek

# Nie vergessen:
stopCluster(cl)
# Noch mehr RAM usage leeren:
gc()
# Das sind die Speicherfresser
sort(sapply(ls(), function(x) object.size(get(x)))/1e6, dec=T) # in MB



# 11.8 Übungsaufgabe -----------------------------------------------------------
# Gegeben sind folgende (fiktive) Daten aus unregelmäßigen Messkampagnen:
UebDat <- read.table("Daten/11_applytest.txt", header=T)
# oder
   File <- "http://dl.dropbox.com/u/4836866/Rclick/Daten/11_applytest.txt"
   UebDat <- read.table(url(File), header=T)
UebDat
# Diese Zeitreihen sollen so als Boxplots dargestellt werden, dass das Skript
# auch mit weiteren Daten laufen wird. Nutzungstypen sind zu berücksichtigen.
# Zudem will man für jede Nutzung und jedes Jahr den Mittelwert mit einzeichen.
# Eine mögliche Lösung findet sich weiter unten, aber probiere doch erst selbst.
# Bessere oder elegantere Lösungen sind - wie immer - willkommen!



View(UebDat)
       
# Parameter fürs Plotten ermitteln
Jahre <- min(UebDat$Jahr):max(UebDat$Jahr)
nJ <- length(Jahre) # Anzahl Jahre
Range <- range(UebDat$Wert) # Wertebereich
Range[2] <- Range[2] + 0.05*diff(Range) # zweiter Wert höher für Anzahlangabe

# Daten nach Landnutzung auftrennen
Dat <- split(UebDat, UebDat$Nutzung)
class(Dat)
str(Dat)
nN <- length(Dat); nN # 2, number of Nutzungen

# Nutzungen je in einem eigenem Fenster darstellen
par(mfrow=c(nN,1), mar=c(2,3,1,1), mgp=c(3,0.5,0))
for(i in 1:nN)
  {
  # evtl NAs in fehlenden Jahren hinzufügen -> Bereich auf allen Achsen gleich 
  fehl <- Jahre[!Jahre %in% Dat[[i]][,"Jahr"]]
  toplot <- Dat[[i]][2:3]
  if(length(fehl)>0) toplot<-rbind(Dat[[i]][2:3], data.frame(Jahr=fehl,Wert=NA))
  # Das eigentliche Plotten
  posit <- boxplot(tapply(toplot[,2], toplot[,1], function(input) input), 
                ylim=Range, main=names(Dat)[i], las=1, col=sample(colors(), nJ))
  points(1:nJ, tapply(toplot[,"Wert"], toplot[,"Jahr"], mean), pch=8)
  text(1:nJ, posit$stats[5,], paste("n =", posit$n), adj=c(0.5,-0.2))
  rm(fehl, toplot, posit, i)
  }# Ende for-Schleife


# Das alles ist jetzt unproblematisch nochmal für folgende Daten möglich:
set.seed(8)
UebDat <- data.frame(Nutzung=sample(c("Wald","Wiese","Acker"), 90, rep=T),
            Jahr=sample(2005:2013, 90, rep=T), Wert=sample(100:300, 90, rep=T) )
# entsprechende Zeilen oben nochmal abschicken...
