### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 15: Extremwertstatistik für Hochwassermaxima #######################

# 15.1 Datensatz
# 15.2 Jährlichkeit berechnen
# Exkurs: Verschiedene Methoden zur Bestimmung der PP
# 15.3 Verteilung anpassen
# 15.4 Zeichnen der Abflüsse über ihre Jährlichkeiten
# 15.5 Abflüsse für bestimmte Jährlichkeiten 
# 15.6 Funktion mit mehreren Verteilungen : L-Momente
# 15.7 Funktion distLextreme
# 15.8 andere Datensätze

# Ich bin kein Experte für Verteilungsfunktionen, sämtliche Kommentare sind also
# bitte mit Vorsicht zu genießen...

# In meinem Paket sind einige hier benötigte Funktionen: movAv
install.packages("berryFunctions") ; library(berryFunctions)

# 15.1 Datensatz ---------------------------------------------------------------
# Jahresmaxima des Abflusses 1976-2010 an einem Bach in Österreich. 
JM <- c(61.5, 77.0, 37.0, 69.3, 75.6, 74.9, 43.7, 50.8, 55.6, 84.1, 43.6, 81.9,
60.1, 72.4, 61.6, 94.8, 82.6, 57.2,  63.1, 73.8, 51.3, 93.6, 56.9, 52.1, 40.4,
48.9, 113.6, 35.4, 40.1, 89.6, 47.8, 57.6, 38.9, 69.7, 110.8)
# Dies müssen unbedingt unabhängig voneinander sein! Es darf keine 
# Jahrestrennung an einem Extremereignis erfolgen (das dann in beiden Jahren
# als Jahresmaxima auftaucht).

# Zeitreihe anschauen
plot(1976:2010, JM, type="l", las=1, ylab="Jahresmaxima Abfluss [m\U00B3/s]")
# Moving Average (in berryFunctions):
lines(1976:2010, movAv(JM), col=2, lwd=3) # kann ein Indikator für Trends oder
# Zyklizitäten sein, aber: Fensterbreite = "researcher degree of freedom"

# Wer die Theorie hinter der Extremwertstatistik schon kennt, kann gleich zu
# 15.7 springen. Ansonsten soll das Folgende einen Eindruck vermitteln...
# Wie immer ist Feedback sehr willkommen!



# 15.2 Jährlichkeit berechnen --------------------------------------------------
quantile(JM) # 75% der Werte sind kleiner als 76.3 m3/s (oberes Quartil)
# Dieser Abfluss wird in 25 von 100 Jahren überschritten, kann also statistisch
# gesehen alle 4 Jahre auftreten (bei gleichbleibendem Abflussregime).

# Bestimmung der Plotting Positions (PP). Früher verwendet zur grafischen
# Anpassung der Extremwertverteilung als Gerade auf Gumbel-skalierter Achse
# Plotting Positions nach Weibull:
m <- sort(JM) # aufsteigend sortierte Maxima
Rang <- 1:length(m)
Pu <- Rang / (length(m)+1) # empirische Unterschreitungswahrsch. = Perzentil
par(mfrow=c(1,2))
plot(m,Pu, xlab="Abflusswert", ylab="Unterschreitungshäufigkeit", las=1)
# Vergleichend die empirische kumulierte Wahrscheinlichkeitsdichte:
plot(ecdf(JM)) # inhaltlich fast das gleiche, jedoch von 0 bis 1, also
# keine Möglichkeit für noch höhere Abflüsse, die es aber gibt.
# wieder zum ersten Plot:
plot(m,Pu, xlab="Abflusswert", ylab="Unterschreitungshäufigkeit", las=1)
# Wiederkehrintervall = Jährlichkeit = 1/Pü = 1/(1-Pu):
RP <- 1/(1-Pu) # RP: Return Period
plot(m,RP, xlab="Abflusswert", ylab="Wiederkehrintervall [Jahre]", las=1)

# Der höchste Abflusswert hat eine Jährlichkeit von 36.
# Der zweithöchste Wert wird in den 35 Jahren zweimal ge- oder übertroffen,
# tritt statistisch gesehen 2x in 35 Jahren, also alle 18 Jahre auf.
# Überschreitung des dritten Wertes passiert alle 12 Jahre. etc.
# Diese Reziproke (1/x) Beziehung bewirkt die unintuitive Abstände zwischen den
# Return Periods.


# Exkurs: Verschiedene Methoden zur Bestimmung der PP --------------------------
browseURL(paste0("http://www.uni-potsdam.de/db/fsr-ggr/data/archiv/klausuren/",
                 "Hydrologie/Extremwertstatistik.pdf"))
browseURL("www.tandfonline.com/doi/abs/10.1080/03610920701653094#.U0UC7FczZqM")
browseURL("http://journals.ametsoc.org/doi/abs/10.1175/JAM2349.1")
browseURL(paste0("http://www.whycos.org/fck_editor/upload/File/Pacific-HYCOS/",
                 "Surface_Waters/Estimating_Flood_Frequency.pdf"))
# Plotting Positions nach Hazen (1914):
# Formeln aus dem dritten Link
Pu.hazen <- (Rang-0.50)/ length(m)
# Plotting Positions nach Gringorten (1963) (wie in lmom:::evplot.default):
Pu.gring <- (Rang-0.44)/(length(m)+0.12)
# Plotting Positions nach Beard (1943):
Pu.beard <- (Rang-0.31)/(length(m)+0.38)
# Plotting Positions nach Weibull (1939):
Pu.weibu <-  Rang      /(length(m)+1)

RP.hazen <- 1/(1-Pu.hazen)
RP.gring <- 1/(1-Pu.gring)
RP.beard <- 1/(1-Pu.beard)
RP.weibu <- 1/(1-Pu.weibu)

legtitles <- c(
"Hazen (1914)     : (Rang-0.50)/ n",
"Gringorten (1963): (Rang-0.44)/(n+0.12)",
"Beard (1943)     : (Rang-0.31)/(n+0.38)",
"Weibull (1939)   :  Rang      /(n+1)")

par(mfrow=c(1,1))
plot( m, RP.hazen, las=1, type="n", main="Methoden für Plotting Positions",
     ylab="Wiederkehrintervall = RP = 1/(1-Pu)", xlab="Abfluss Jahresmaxima")
lines(m, RP.hazen, col=1, type="o", lty=1, pch=1)
lines(m, RP.gring, col=2, type="o", lty=2, pch=2)
lines(m, RP.beard, col=3, type="o", lty=3, pch=3)
lines(m, RP.weibu, col=4, type="o", lty=4, pch=4)
par(family="mono")
legend("topleft", legtitles, col=1:4, lty=1:4, pch=1:4, title="P_unterschr. =")
par(family="")
# macht bei den hohen Werten einen großen Unterschied!
# Entsprechend dann auch in der Berechnung der n-jährlichen Abflüsse!
# Hazen, Gringorton und Beard überschätzen Wiederkehrinterval und somit
# unterschätzen sie das Risiko! (verglichen mit Weibull)

# I tend to like the Weibull method for Plotting Positions more.
# With 35 years of data, Gringorton estimates a return period of 60 years.
# But if we were to take n -> Inf samples of 35, in the long run, the highest of
# these values should have a recurrence interval of ~35 years.



# 15.3 Verteilung anpassen -----------------------------------------------------
# Abflussvektor für die Graphik Qrg <- extendrange(m, f=0.1)
Q <- seq(par("usr")[1], par("usr")[2], len=500) 

# Parameter der Gammaverteilung aus den Momenten der Daten bestimmen
shape <- mean(m)^2/var(m)
rate  <- mean(m)/var(m) 
# Die plotting positions sind für das fitten der Verteilungen irrelevant.

# Verteilung über den Bereich Q (Graphikbereich an Abflüssen)
hist(m, freq=FALSE, col=3)
lines(Q, dgamma(x=Q, shape=shape, rate=rate), lwd=2)
# Trifft halbwegs zu... Allerdings ist das Tailing der Werte ausgeprägter!

# Neben der Gammaverteilung gibt es noch eine Menge andere, zB GEV, GPD, Gumbel,
# normal, exponential, logistic, usw. Siehe Abschnitt 15.6 bis 15.8



# 15.4 Zeichnen der Abflüsse über ihre Jährlichkeiten --------------------------
plot(RP, m, ylab="Discharge HQ  [m\U00B3/s]", xlab="Return Period RP  [a]",
     las=1, main="yearly Discharge Extrema / Return Period", pch=20)
# Return Period für Abflussvektor nach der so parametrisierten Gammaverteilung:
RPgamma <- 1/( 1-  pgamma( Q, shape=shape, rate=rate)    )
# Einzeichnen der Extremwertverteilung
lines(RPgamma, Q)
legend("right", legend=c("Jahresmaxima 1976-2010", "Gammaverteilung"), 
      lty=c(NA,1), pch=c(20,NA))
# Schreiben der Parameter
text(15,60, paste("mean =",round(mean(m),3),"  sd =",round(sd(m),3)), adj=0 )
text(15,53, bquote("shape = "* mu^2/sigma*" = "*.(round(shape, 2))), adj=0 )
text(15,46, bquote("rate = " * mu/sigma * " = "*.(round(rate,  4))), adj=0 )
text(15,39, bquote("scale = "* 1/rate *   " = "*.(round(1/rate,3))), adj=0 )
# Auch hier wird das Unterschätzen hoher Abflüsse deutlich!
# Die Achsen sind bewusst linear skaliert - das kann man sich visuell viel 
# besser vorstellen.



# 15.5 Abflüsse für bestimmte Jährlichkeiten -----------------------------------
ReturnYears <- c(5,10,20,50)
qgamma(1-(1/ReturnYears), shape=shape, rate=rate)



# 15.6 L-Momente ---------------------------------------------------------------
if(!require(lmom)){install.packages("lmom");require(lmom)} # samlmu, pelgev
if(!require(evd)){install.packages("evd");require(evd)} # dgev

# Lineare Momente der Stichprobe
momente <- samlmu(JM, sort.data=TRUE, nmom=3)
momente
browseURL("http://en.wikipedia.org/wiki/L-moment")

# Parameter der General Extreme Value Distribution anhand der L-Momente schätzen
param <- pelgev(samlmu(JM)) # PEL: Parameter Estimation L-moments
param  # Die Hilfe zu pelgev hat eine Übersicht der implementierten Verteilungen

# Angepasste General Extreme Value Distribution einzeichnen:
hist(JM, col=4, freq=FALSE, breaks=10)
lines(density(JM), lwd=1, lty=2)
x <- seq(20,130, len=100)
lines(x, dgev(x=x,loc=param["xi"],scale=param["alpha"],shape=param["k"]), lwd=3)
# Verteilung ist mäßig geeignet, auch die anderen sind auszuprobieren

# Quantil der so parametrisierten GEV Verteilung (analog zu qnorm):
quagev(f=0.99, param) # 99% der Werte dieser GEV Verteilung liegen unter 127.5

# Extreme Value plot mit nicht-linearer Gumbel-Achse:
evplot(JM, rp.axis = TRUE, ylab= "Discharge [m\U00B3/s]")
evdistq(qfunc=quagev, param, col ="black")

# Wert der Verteilung für ReturnPeriod 50 Jahre finden:
quagev(f=1-1/50, para=pelgev(momente)) # aus exp(-exp(log(-log(1-1/50))))
quagam(f=1-1/50, para=pelgam(momente))
# Unterschiedliche Datensätze brauchen unterschiedliche Verteilungen.
# Die GEV passt nicht immer am besten!

# Unsicherheit der Parameter bestimmen via Bootstrapping, hier mit Leave5Out:
q99gev <- sapply(X=5:length(JM), FUN=function(i){
   param <- pelgev( samlmu(JM[-((i-4):i)], sort.data = TRUE) )
   evdistq(qfunc=quagev, param, col ="black") # Verteilung mit einzeichnen
   quagev(0.99, param) })   # 99% Quantil bestimmen und rausgeben
# Da sind schon Abweichungen in der Graphik erkennbar!
# Wenn man für die Berechnung des Hochwassers einer gewissen Jährlichkeit einen
# Unsicherheitsbereich angeben will, ist das eine der Methoden.
as.numeric(q99gev)
hist(q99gev, breaks=15, col=3)

# Mögliche andere Verteilungen - Generalized Pareto Distribution (GPD):
plot(ecdf(JM))
lines(x, cdfgpa(x, pelgpa(samlmu(JM))), col=2) # Naja, geht so
quagpa(0.99, pelgpa(samlmu(JM))) #99% der Werte dieser GPD sind < 113.6
# Das ist schon ein wenig anders als 127.5!

# To-Do: wie relevant werden Verteilungen durch L-momente anders bestimmt
# als durch klassische statistische Momente?



# 15.7. Funktion distLextreme --------------------------------------------------
install.packages("devtools")
devtools::install_github("brry/extremeStat")
library(extremeStat)
library(lmomco)
distLextreme(JM)

# 50-jähriges Ereignis anhand verschiedener Verteilungen abschätzen:
Q50 <- distLextreme(JM, RPs=50, plot=FALSE)$returnlev
sort(Q50[,1], decr=TRUE)
# das ist eine ganze Bandbreite an Möglichkeiten...
# 107 oder 132 m3/s kann schon einen relevanten Unterschied machen!

# Farben der Linien und Auswahl zu plottender Verteilungen:
dle <- distLextreme(JM)
distLextreme(dlf=dle, coldist=heat.colors(10), nbest=15)
distLextreme(dlf=dle, coldist=heat.colors(13), selection=c(4,9,1))
distLextreme(dlf=dle, coldist=1:3, selection=c(4,9,1))
distLextreme(dlf=dle, legarg=list(cex=0.5, x="bottom", box.col="red", col=3))

# Nach Anpassungsgüte gewichtetes Mittel der verschiedenen Verteilungen:
?distLextreme # Section Examples



# 15.8 andere Datensätze -------------------------------------------------------
# MackenzieRiver
MR <- c(26600,30300,34000,32000,29200,28300,28600,26400,28300,28800,
  29000,22100,32900,31800,21600,32100,27000,24800,28000,35000,32000,25000,15800,
  28800,29900,28000,25600,19700,25700,29500,26800,30000,29500)
?Nile # 100 years!

evplot(MR, ylab= "Discharge [m\U00B3/s]")
evdistq(qfunc=quagev, para=pelgev(samlmu(MR)), col ="black")

extremeStatLmom(MR)
extremeStatLmom(MR, returnParam=T)

extremeStatLmom(Nile)

