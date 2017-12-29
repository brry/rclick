### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 13: Zeit und Datum #################################################

# 13.1 Datumsformat einlesen und formatieren
# 13.2 Zeitformat einlesen und formatieren
# 13.3 Plot und Achsenbeschriftung
# 13.4 Zeitreihen aggregieren
# 13.5 Hydrologisches Jahr
# 13.6 nicht-equidistante Zeitreihen
# 13.7 ?bungsaufgaben
# 13.8 L?sungen ?bungsaufgaben


# Beispieldaten erzeugen:
Daten <- read.table(as.is=T, header=T, text="
Datum Zeit Wert
21.01.2016 13:30:00 19
21.01.2016 13:45:00 28
21.01.2016 14:00:00 17
21.01.2016 14:15:00 29
21.01.2016 14:30:00 24
21.01.2016 14:45:00 34
21.01.2016 15:00:00 28
21.01.2016 15:15:00 12")


# 13.1 Datumsformat einlesen und formatieren -----------------------------------
zeit1 <- as.Date(Daten[,1], format="%d.%m.%Y")
# alternative Formate "%m-%d-%Y" "%Y/%m/%d" usw.
class(zeit1) ; as.numeric(zeit1) # intern ganze Zahlen f?r Tage seit 1970-01-01
as.Date("20090921","%Y%m%d")    # geht auch ohne Leerzeichen

xyz <- c("2014-02","2014-04","2015-07","2015-11")
# Datumsreihen im Format mm.yyyy einlesen geht nicht mit
as.Date(             xyz ,    "%Y-%m") # sondern nur mit
as.Date(paste0("01.",xyz), "%d.%Y-%m") # Datumsobjekte brauchen immer einen Tag

# locale-specific sind Sachen wie %A, die auf jedem Sys.getlocale() anders sind:
format(Sys.Date(), "%a %d.%b.%Y (= ein %A im %B)")
# Mehr Formate unter Details in der Hilfe von
?strptime
format(Sys.time(), "  %H Uhr %M")
format(Sys.Date(), "%A, der %d. %b im Jahre %Y")

tenweeks <- seq(Sys.Date(), length.out=10, by="1 week") # next ten weeks
tenweeks
weekdays(Sys.Date())
months(tenweeks)

# Exakt 1 Jahr zum Datum dazu addieren:
k1 <- as.Date("15.3.2007", "%d.%m.%Y")
k1
k1 + 1
k1 + 365 # geht nicht bei Schaltjahren. workaround:
k2 <- paste(as.numeric(format(k1, "%Y"))+1  ,   format(k1, "%m-%d") , sep="-"  )
k2 # es gibt aber bestimmt eine einfachere L?sung...
k2 + 7  # geht noch nicht, denn:
mode(k2) # k2 ist kein Datum, sondern character
k3 <- as.Date(k2, "%Y-%m-%d")
k3
mode(k3)  # zeigt numeric, denn intern ist ein Datum, wie bei Excel, eine Zahl
class(k3)
k3 + 7



# 13.2 Zeitformat einlesen und formatieren -------------------------------------
# Bei Uhrzeit  (strptime bedeutet sowas wie string parsed time)
zeit2 <- strptime(Daten[,2], format="%H:%M:%S") # format ist fuer input
# automatisch wird das aktuelle Datum hinzugefuegt
class(zeit2) # Portable Operating System Interface = POSIX: internat. Standard
as.numeric(zeit2) # Ganze Zahlen fuer Sekunden seit 1970-01-01 01:00:00 CET


# Bei Datum und Uhrzeit in 2 Spalten: Spalten mit paste zusammenf?gen
Zeit <- strptime(  paste(Daten[,1],Daten[,2])  , "%d.%m.%Y %H:%M:%S")
data.frame(Zeit)
str(Zeit) # ein POSIX long time Objekt

# Abk?rzungen: "%Y-%m-%d %H:%M"  =  "%F %R"
format(Sys.time(), "%F %T") # "2014-12-04 12:49:37"

# Ausgabe formatieren (z.B. f?r Achsenbeschriftungen)
format(Zeit, "%H:%M")
# alternativ l?sst sich hier auch strftime (string format time) benutzen.
strftime(Zeit, "%d.%m., %H:%M:%S") #format ist f?r output
?strftime # f?r eine ganze Reihe von Formatvorlagen inkl Wochentag etc.

# Einzelne Komponenten aus Daten herausnehmen:s
format(Zeit, "%M")
which( format(Zeit, "%M")==15 )
Daten[ format(Zeit, "%M")==15 , ]

# Zeitreihen
anfa <- as.Date("2007/10/18")
ende <- as.Date("2007/11/05")
anfa; ende
seq(anfa, ende, by="day") # beruecksichtigt Schaltjahre
strftime(anfa, "%b")
strftime(anfa, "%B")

# Namen von Monate / Tage auf Englisch:
x <- Sys.Date()+1:12*30
format(x, "%b, %B, %a, %A")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
format(x, "%b, %B, %a, %A")
Sys.setlocale("LC_TIME", lct) # Linux: "en_US.UTF-8" Windows: "English"
# for explicitly setting to German (OS-dependant!), see the examples in:
?Sys.setlocale

Sys.setlocale(category = "LC_TIME", locale="C")
format(Sys.Date(), "%B")
Sys.setlocale(category = "LC_TIME", locale="") # Alternative for resetting locale


# Zeitobjekt in Spalte einf?gen ging fr?her nicht bei POSIXlt
Daten$Zeit2 <- Zeit
Daten$Zeit2 <- as.POSIXct(Zeit) # h?tte man fr?her gebraucht...



# 13.3 Plot und Achsenbeschriftung ---------------------------------------------
plot(Zeit, Daten[,3], type="l")
# Selber mit axis() Achsen beschriften ist problematisch.

# bei reinem Datum geht plotten und beschriften noch recht einfach
y <- c(54, 63, 48)
x <- as.Date(c("2010-08-05", "2010-08-12", "2010-08-14")) # Default format
plot(x, y)
plot(x, y, xaxt="n")
axis(1, as.Date("7.8.2010","%d.%m.%Y"), expression( bold(hier) )  )
axis(1, x, format(x,"%d. %b\n%Y"))
plot(x, y, xaxt="n")
axis(1, pretty(x), format(pretty(x),"%d.%b %y"))

# automatisch: Labels an regelm??igen Tagen des Monats:
if(!require("berryFunctions", quietly=TRUE)) install.packages("berryFunctions")
library(berryFunctions)
# source(url("http://dl.dropbox.com/u/4836866/berryFunctions/R/monthLabs.R"))
plot(as.Date(c("2013-01-05", "2015-06-9")), 1:2)
plot(as.Date(c("2013-01-05", "2015-06-9")), 1:2, xaxt="n")
monthAxis(npy=2, format="%d.%m.%y")
plot(as.Date(c("2013-01-05", "2015-06-9")), 1:2, xaxt="n")
monthAxis(ym=TRUE, tcl=1)


# bei Uhrzeit geht das mit axis.POSIXct
y2 <- c(54,63,48)
d2 <- c("05.08.2010 12:13:07", "05.08.2010 12:18:54", "05.08.2010 12:29:14")
x2 <- strptime(d2, "%d.%m.%Y %H:%M:%S")
plot(x2, y2)   # X-Achsenbeschriftung in Stunden, Minuten und Sekunden
plot(x2, y2, xaxt="n")
k <- "2010-08-05 12:17:00 MEZ"
# automatisiert:      (braucht CET: CentralEuropeanTime statt MEZ)
k2 <- as.POSIXct("05.08.2010 12:26:07", "%d.%m.%Y %H:%M:%S", tz="CET")
axis.POSIXct(1, at=k,  format="%H:%M:%S")
axis.POSIXct(1, at=k2, format="%H:%M:%S")
axis.POSIXct(1, at="2010-08-05 12:22:00 MEZ", labels="hier")

# oder normal mit axis, wenn mit POSIXct-Objekten gearbeitet wird:
plot(x2, y2, xaxt="n")
axis(1, at=k2, labels=format(k2, "%H:%M:%S") )
abline(v=as.POSIXct("2010-08-05 12:22:00 MEZ"), lty=2)
# Valid time zones:
browseURL(paste0("http://vmware.ie/support/developer/vc-sdk/visdk400pubs/",
                 "ReferenceGuide/timezone.html"))



# 13.4 Zeitreihen aggregieren --------------------------------------------------
zr <- strptime(c("05.08.2010 12:15", "09.02.2013 14:45"), "%d.%m.%Y %H:%M")
zr <- seq(zr[1], zr[2], by=15*60) # Sequenz in 15-Minuten-Schritten
head(zr)
tail(zr)
fd <- cumsum(rnorm(length(zr))) # fake data aus Random Walk

plot(zr, fd, type="l")
plot(zr, fd, type="l", xaxt="n", main=c("t?glicher Random Walk",""))
axis.POSIXct(1, as.Date(paste0(2010:2015,"-01-01")), labels=NA)
yearlabs <- as.Date(paste0(2010:2015,"-07-01"))
axis.POSIXct(1, at=yearlabs, labels=format(yearlabs, "%Y"), tick=F)

# Monatliche Mittelwerte ausrechnen (aggregieren):
monmean <- tapply(fd, format(zr, "%Y-%m"), mean) # apply the function mean on
monmean    # the values fd that are first categorized into groups by month
# Mittelpunkt des Monats als Datumsobjekt:
monmid <- as.POSIXct(paste0(names(monmean), "-15"), format="%Y-%m-%d")
lines(monmid, monmean, col=2, lwd=2)
title(main=c(""," mit Monatsmittel"), col.main=2, cex.main=0.9)


# Summarize daily records by quarter of the year
set.seed(1492)
logins <- data.frame(date=as.Date("2014-01-01")+0:364,
                     failures=round(rlnorm(365)*20)   )
# Extract month and quarter:
logins$month <- strftime(logins$date, "%m")
logins$qtr <- ceiling(as.numeric(logins$month)/3)
# Summarize by quarter:
tapply(X=logins$failures, INDEX=logins$qtr, FUN=sum)
tapply(X=logins$failures, INDEX=logins$qtr, FUN=median)


# unregelm??ige Messungen zu viertelst?ndlichen Werten aggregieren:
ourdata <- data.frame(Zeit=as.character(sort(Sys.time()+rnorm(15, sd=60*30))),
                      Wert=rexp(15))
str(ourdata)
times <- strptime(ourdata[,1], format="%Y-%m-%d %H:%M", tz="Asia/Manila")
# Achtung: sekunden werden hier einfach ignoriert, nicht gerundet:
times$min <- ceiling(times$min/15)*15
tapply(ourdata$Wert, as.POSIXct(times), FUN=sum, na.rm=T)
# f?r Niederschlagswerte sum, f?r Temperaturen mean, f?r Abflussmaxima max, etc.



# 13.5 Hydrologisches Jahr -----------------------------------------------------
# Anfang hydrologisches Jahr in Deutschlands Klima: 1. November
zr <- strptime(c("05.08.2010 12:15", "09.02.2013 14:45"), "%d.%m.%Y %H:%M")
zr <- seq(zr[1], zr[2], by=60*60) # Sequenz in Stunden-Schritten
set.seed(007)
fd <- cumsum(rnorm(length(zr))) # fake data aus Random Walk
plot(zr, fd, type="l", las=1)
berryFunctions::monthAxis()
abline(v=as.POSIXct(as.Date(paste0(2010:2013,"-11-01"))), col=2)
abline(v=as.POSIXct(as.Date(paste0(2010:2013,"-01-01"))), col=4)
tapply(fd, format(zr+61*24*60*60, "%Y"), max) # +61 if Date format!




# 13.6 nicht-equidistante Zeitreihen -------------------------------------------
# kumulierte Niederschlagswerte
set.seed(1234) # Startwert f?r den Zufallszahlengenerator (Reproduzierbarkeit)
raindata <- data.frame(time=strptime("2011-01-28 02:08", "%Y-%m-%d %H:%M") +
                                      sort(sample(1:(400*24*3600), 150)),
                       rain=sample(0:10, 150, replace=TRUE))
head(raindata)
tail(raindata)

par(mfrow=c(3,1), mar=c(4,2.5,1,0.5))
plot(raindata[  1: 50,], xlab="2011", type="h", col=4, las=1)
plot(raindata[ 51:100,], xlab="2011", type="h", col=4, las=1)
plot(raindata[101:150,], xlab="2011/2012", xaxt="n", type="h", col=4, las=1)
monthAxis( format="%b" )
# "zeitlich geb?ndelte" Regenf?lle erkennbar

monthly_rain <- tapply(raindata$rain, format(raindata$time, "%Y-%m"), sum)
monthly_rain
monthly_rain <- monthly_rain[-1]   # Januar entfernen, weil nur 1 Wert

par(mfrow=c(1,1), mar=c(5.5,4,2,0.5), lend="butt")
plot(monthly_rain, type="n", las=1, xlab="", ylab="Niederschlagssumme",
     xaxt="n", main="Niederschlag", mgp=c(2.5, 1, 0),
     ylim=c(0, max(monthly_rain)*1.04), yaxs="i")
abline(h=0:6*20, col=8) ; box()
points(monthly_rain, type="h", col=4, lwd=11) # kann man auch mit barplot machen
axis(1, 1:13, substr(names(monthly_rain),6,7), tick=F, mgp=c(3,.5,0))
axis(1, c(6,12), substr(names(monthly_rain),1,4)[c(6,12)], tick=F, mgp=c(3,1.5,0))
title(xlab="Monat", mgp=c(4,1,0)) # mgp[1] ist f?r Abstand des Achsentitels



# 13.7 ?bungsaufgaben ----------------------------------------------------------
# 1. In welchen Jahren seit der Wende war der 15.10. ein Mittwoch?
# (L?sst sich mit 75 Zeichen in zwei Kommandozeilen herausfinden)

# 2. Ignorieren Sequenzen mit 20 minuten Abstand die Schaltsekunden?

# 3. Lese ein Datum ein, das wie folgt formatiert ist: "110809" (9.Aug.2011)
# Ausgehend davon, erstelle eine Sequenz von einer Stunde im Sekundenabstand

# 4. Schreibe eine Funktion die abh?ngig vom Geburtszeitpunkt dein Alter in
# Sekunden, Minuten, Stunden, Tage, Wochen, Monate, Jahre ausgibt.
# Optional: formatiere alle zahlen schick rechtsb?ndig untereinander.
# Stelle fest, wann du runde Jubil?en in den jeweiligen Einheiten feierst.
# Wann ?berholte dein Alter in Minuten die Weltbev?lkerung?

# 5. Oftmals sind lange Zeitreihen nicht ?quidistant:
# an welchen Stellen liegen Unterbrechungen vor und wie gro? sind diese?
# Gib den Unterschied auch in Stunden an.
anfang <- strptime("05.08.2010 18:15:00", "%d.%m.%Y %H:%M:%S")
set.seed(42) ; zeitreihe1 <- anfang + c(0:5*15, sort(runif(25, 60, 600000)))
# Zeige die Zeitpunkte nach denen Messungen fehlen, mit Dauer der L?cke dahinter
zeitreihe2 <- ( anfang + 0:200*5*60 ) [-runif(30,1,201)]



# 13.8 L?sungen ?bungsaufgaben -------------------------------------------------
# Aufg1: Wochentage
d <- as.Date(paste(1990:2014, "10-15", sep="-"))
d[ format(d, "%a") == "Mi"]   # 1997, 2003, 2008, 2014


# Aufg2: Sequenzen, Schaltsekunden
tail(.leap.seconds)
kk <- strptime(c("2012-06-30 23:20", "2012-07-01 03:20"), "%Y-%m-%d %H:%M")
seq(from=kk[1], to=kk[2], by=20*60 ) # ignoriert sie (Minuten sind noch Rund)
kk[1] + 1:12*20*60 # ditto
data.frame(k=strptime("2012-07-01 01:59:50", "%Y-%m-%d %H:%M:%S") + 1:20)
# Mein System ignoriert die, wie erkl?rt unter Details der Hilfe zu
?.leap.seconds


# Aufg3: Formatkonvertierungen
myDate <- as.POSIXct("110809", format = "%y%m%d")
myDate
head(myDate + 0:3600)


# Aufg4: Alter
alter <- function(geburt, abfrage=Sys.time(), units=c("s","m","h","d","w"))
{
weite <- function(k)
             {ifelse(k>=1e9, 14, ifelse(k>=1e6, 15, ifelse(k>=1e3, 16, 17)))}
oldopt <- options(scipen=10)
for (i in 1:length(units))
    {
    k <- difftime(abfrage, strptime(geburt, "%Y-%m-%d %H:%M"), units=units[i])
    cat(format(k, big.mark=" ", nsmall=2, digits=2, width=weite(k)), "\n")
    }
k <- difftime(abfrage, strptime(geburt, "%Y-%m-%d %H:%M"), units="d")
cat(format(as.numeric(k2 <- k/(365.2425/12)), big.mark=" ", nsmall=2,
           digits=2, width=weite(k2)), "months \n")
k <- difftime(abfrage, strptime(geburt, "%Y-%m-%d %H:%M"), units="d")
cat(format(as.numeric(k2 <- k/365.2425), big.mark=" ", nsmall=2,
           digits=2, width=weite(k2)), "years \n")
options(oldopt)
}

alter(geburt="2005-08-11 07:05")

pretty(736435817.66)

1e9/3600/24/365.2425  # 1 billion (dt: milliarde) sekunden in 31.7 Jahren
13e6 /60/24/365.2425  # 13 Mio Minuten bei 24.7 Jahren


# Aufg5: L?cken
diff(zeitreihe1)
difftime(zeitreihe1[-1], zeitreihe1[-length(zeitreihe1)], units="h")

data.frame(Luecke_Nach=zeitreihe2[-length(zeitreihe2)],
           Lueckenlaenge=diff(zeitreihe2))[diff(zeitreihe2)!=5,]

