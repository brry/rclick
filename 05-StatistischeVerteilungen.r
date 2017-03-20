### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 5: statistische Verteilungen #######################################
# Wahrscheinlichkeitsverteilungen (Probability distributions)

# Verteilungen                         Parameter:
# 5.1 diskret: Binomialverteilung      Stichprobenumfang (n), Wahrsch. (p)
# 5.2 diskret: Poissonverteilung       Mittelwert (m)
# 5.3 stetig: Normal / Gauss           Mittelwert (m), Standardabweichung (sd)
# 5.4 stetig: Exponentialverteilung    Rate (r)
# 5.5 stetig: Beta-Verteilung          shape1 (a/alpha), shape 2 (b/beta)

# 5.6 weitere Verteilungen
# 5.7 Simulationen

# Vorsilben (Prefixes):                                             Beispiel:
# d probability density function     Wahrscheinlichkeitsfunktion    dexp(...)
# p	cumulative probability function  Verteilungsfunktion            pbinom(...)
# q	quantiles of the distribution                                   qnorm(...)
# r	random numbers generated from the distribution                  rpois(...)


# 5.1 Diskret: Binomialverteilung ----------------------------------------------
# Beispiele: Torwandschieﬂen; Erfolg = Treffer,     Misserfolg = kein Treffer
# Ziehen von Kugeln aus Urne; Erfolg = weiﬂe Kugel, Misserfolg = schwarze Kugel
# Relevant ist die Anzahl der Erfolge in n Versuchen
# Wenn es um das Ergebnis eines einzelnen Versuches geht: Bernoulli-Verteilung

# Werte definieren
werte <- seq (from=0, to=30, by=1)        # kurz: seq(0,30,1)   # k¸rzer 0:30
werte

# Binomial-Wahscheinlichkeits-Dichte-Funktion:
y.dbinom <- dbinom(x=werte, size=10, prob=0.5)     # 10 mal ziehen,
plot(werte, y.dbinom)                  # Erfolgswahrscheinlichkeit gleich 0,5
?dbinom                                # (Trefferquote ist 50 %)

# ˆfter ziehen
plot(werte, dbinom(werte, size=10, prob=0.5), ylim=c(0,0.25), pch=16, 
     main="Binomialverteilung, Erfolgswahrs. = 0.5", xlab="Anzahl Erfolge")
points(werte, dbinom(werte, 20, 0.5), ylim=c(0,0.25), pch=16, col=2)
points(werte, dbinom(werte, 50, 0.5), ylim=c(0,0.25), pch=16, col=3)
legend("topright", paste("n =", c(10,20,50)), title="Anzahl Versuche", pch=16, 
       col=1:3, inset=0.02)
                                                                  
# Erfolgsquote ver‰ndern
plot(werte, dbinom(werte, 20, 0.85), ylim=c(0,0.25), pch=16, 
     main="Binomialverteilung, Stichprobenumfang n = 20", xlab="Anzahl Erfolge")
points(werte, dbinom(werte, 20, 0.5), ylim=c(0,0.25), pch=16, col=2)
points(werte, dbinom(werte, 20, 0.2), ylim=c(0,0.25), pch=16, col=3)
legend("topright", paste("prob =",c(.85,.5,.2)), title="Erfolgswahrsch.",
       pch=16, col=1:3, inset=0.02)

n <- 8 # Anzahl M¸nzw¸rfe: wie wahrscheinlich x mal Kopf?
plot(0:n, dbinom(0:n, n, 0.5), pch=16, xlab="x = Anzahl 'Kopf'", las=1,
     main=paste(n, "-facher M¸nzwurf", sep=""), ylab="Wahrsch. f¸r x-fach Kopf")
abline(h=0.025, col=6)
# Bei 8-maligem Werfen kommt es in 3% der Versuche vor, dass nur 1 mal Kopf
# auftritt. Wenn jemand 8 mal wirft, und nur 1 mal Kopf (bzw. Zahl) auftritt,
# kann ich dennoch nicht annehmen, dass die M¸nze gezinkt ist (bei 2.5%-Grenze).

# Mord in Palermo / Werwˆlfe
werte <- 0:10
windows(record=T)
for (i in 0:40) { plot(werte, dbinom(werte, i, 1/8), ylim=c(0,1), xlab="x",
main=paste("Wahrscheinlichkeit, Mˆrderkarte x mal zu bekommen\nbei",i,"Runden"))
legend("topright", "1 von 8 Karten Mˆrder") }


# Die Zufallsvariable X sei 'Anzahl Sechser beim viermaligen W¸rfeln'.
# Die Einzelwahrscheinlichkeiten der f¸nf mˆglichen Realisationen sind
# P(X=0) =           (5/6)^4 = 0.48225
# P(X=1) = 4ï(1/6)ï  (5/6)^3 = 0.38580
# P(X=2) = 6ï(1/6)^2ï(5/6)^2 = 0.11524
# P(X=3) = 4ï(1/6)^3ï(5/6)   = 0.01543
# P(X=4) =   (1/6)^4         = 0.00077
dbinom(0:4, 4, 1/6)
plot(0:4, dbinom(0:4, 4, 1/6), pch=16)

# Zufallszahlen ziehen
rbinom(n=40, size=10, prob=0.5)



# 5.2 Diskret: Poissonverteilung -----------------------------------------------
# Z‰hldaten, z.B. Anzahl Telefonanrufe pro Stunde im Callcenter
# oft auch bei geringen Wahrscheinlichkeiten f¸r groﬂe Werte (p klein, n groﬂ)
# Beispiel: radioaktiver Zerfall; Aus einer sehr groﬂen Anzahl von Atomen
# zerf‰llt in einer Zeiteinheit nur ein sehr kleiner Anteil der Atome.
# Dieser Zerfall ist zuf‰llig und unabh‰ngig von den schon zerfallenen Atomen.

# Binomial-Wahscheinlichkeits-Dichte-Funktion
# lambda ist mittleres Auftreten von Ereignissen
plot(0:20, dpois(0:20, lambda = 0.5), pch=16)
points(0:20, dpois(0:20, lambda = 1), pch=16, col=2)
points(0:20, dpois(0:20, lambda = 2), pch=16, col=3)
points(0:20, dpois(0:20, lambda = 5), pch=16, col=4)
points(0:20, dpois(0:20, lambda =10), pch=16, col=5)

# Zufallszahlen ziehen
rpois(n=5, lambda=0.8)



# 5.3 Stetig: Gauss-(Normal)-verteilung ----------------------------------------
# analog zum Vorgehen bei den stetigen Verteilungen
werte <- seq (from = -10, to = 10, by = 0.01)
head(werte)
plot(werte)
plot(werte, dnorm(werte, mean = 5, sd = 1.0))

windows(record=T) # Speichere Bilder, sodass mit Pg Up und Pg Dn durchklickbar
for(i in 1:10) # f¸hre f¸r jedes i folgende Zeilen aus:
  {
  plot(-20:20, dnorm(-20:20, 0, sd=i ), main=paste("St. Abw. =", i),
       xlab="x", ylab="p(x)", type="o")
  abline(v=c(0+i, 0-i))
  } # Mehr zu For-Schleifen in Kapitel 10

dev.off() # schlieﬂe Graphikfenster

# Zufallszahlen als Spalte
zuf.zahl <- rnorm(10, mean = 5.0, sd = 0.5)
data.frame(zuf.zahl)
rnorm(10)

# Plotten von Zufallszahlen der Standardnormalverteilung (mean=0, sd=1)
plot(rnorm(10))
plot(rnorm(1000), pch=16, cex=0.5)
plot(rnorm(500,mean=0,sd=1),rnorm(500,mean=0,sd=1),
     pch=16,cex=0.5,abline(v=0, h=0),xlim=c(-4,4), ylim=c(-4,4))


# Variationskoeffizient
x <- -1:130
plot(x, dnorm(x,5,2), type="l", main="Unsinn des Variationskoeffizienten", 
     ylab="dnorm(x,5,2)   |    dnorm(x,100,2)")
lines(x, dnorm(x,100,2))
text(10,  0.15, "CV = sd/mean = 0.4", adj=0)
text(105, 0.15, "CV = 0.02",adj=0)
# VK also nur bei Vergleichen zweier Gruppen mit ‰hnlichem Mittelwert sinnvoll


set.seed(123)  # Gibt dem "Zufallszahlen"-Generator den Startpunkt, sodass
# jeder Lauf die gleiche Zahlen ergibt.
d <- rnorm(29); hist(d)

# Homer's Favorite Food - normalverteilt angen‰hert...
par(mar=c(0,0,0,0), pch="." )
plot(10,10, xlim=c(0,20), ylim=c(0,20))
points(  rnorm(100000, 10, 2) + 5*sin(seq(0, 2*pi, len=100000)), 
         rnorm(100000, 10, 2) + 5*cos(seq(0, 2*pi, len=100000))  )
# Points-Befehl h‰ufiger wiederholen...

# Anwendung kumulierter Wahrscheinlichkeit (pnorm)
# Umfrage zur Kˆrpergrˆﬂe von Personen
# Mittelwert 170 cm, Standardabweichung 8 cm in Normalverteilung
graphics.off()
plot(150:190, dnorm(150:190, mean=170, sd=8), type="l") ; #abline(h=0:5/100)
par(new=T); plot(150:190,pnorm(150:190,170,8), type="l", axes=F, col=2, ylab="")
axis(4,pretty(0:1), col.axis=2) ; abline(v=c(160,185, 165.8), col=c(4,4,3))
for(h in 1:9*.1) lines( c(qnorm(h,mean=170, sd=8),200), rep(h,2), col=2)

# Wahrscheinlichkeit, dass eine zuf‰llig gew‰hlte Person kleiner als 160 ist
pnorm(160, mean=170, sd=8)

# Wahrscheinlichkeit, dass eine zuf‰llig gew‰hlte Person grˆﬂer als 185 ist
1 - pnorm(185, mean=170, sd=8)

# Wahrscheinlichkeit, dass eine Person zwischen 160 und 185 groﬂ ist
pnorm(185, mean=170, sd=8) -  pnorm(160, mean=170, sd=8)

# Mit 30%iger Wahrscheinlichkeit ist eine Person kleiner als ...
qnorm(.3, mean=170, sd=8)



# 5.4 Stetig: Exponentialverteilung --------------------------------------------
# Beispiel: Zeitabstand zwischen zwei Erdbeben / Naturkatastrophen
# Beispiel: Lebensdauer von elektronischen Bauelementen;
# Die Wahrscheinlichkeit, dass ein ‰lteres Bauteil noch t Tage h‰lt,
# muss genauso groﬂ sein, wie die, dass ein neues Bauteil t Tage h‰lt.
# Die "Ausfallrate" ist konstant (nur f¸r positive Werte).

# analog zum obigen Vorgehen
werte <- seq (from = 0, to = 1, by = 0.001)
plot(werte, dexp(werte, rate = 3), type="l", main="Exponentialverteilung")
lines(werte, dexp(werte, rate = 5), col=2)
text(0.3, 2.5, "rate=5", col=2)  ; text(0.1, 1.5, "rate=3")

# Zufallszahlen
rexp(8, rate = 3)



# 5.5 Stetig: Beta-Verteilung --------------------------------------------------
# Zwischen 0 und 1; wird viel in Bayes-Statistik verwendet
werte <- seq (from = 0, to = 1, by = 0.001)
plot(werte, dbeta(werte, shape1=0.5, shape2=5))

# Zufallszahlen
rbeta(8, shape1=3, shape2=1)

# grafisch dargestellt
browseURL("http://www.uni-konstanz.de/FuF/wiwi/heiler/os/vt-index.html")
# Siehe auch library("berryFunctions"); ?betaPlot


# Normalverteilungen beschreiben Werte nicht immer richtig! (meistens nicht):
set.seed(2) ; d <- rbeta(200, 2,4)*350 + 80
hist(d, col=3, breaks=20)
text(350,25, paste("mean =", round(mean(d),2),"\nsd =",round(sd(d),2)), adj=1)
par(new=T)
plot(80:400, dnorm(80:400, mean(d),sd(d)), type="l", lwd=3, axes=F, ann=F)
# wo die Normalverteilung die meisten Werte erwartet, sind weniger!



# 5.6 weitere Verteilungen -----------------------------------------------------
werte <- seq(-3, 3, .01) 

plot(werte, dunif(werte, -1, 1), type="l") # density uniform distribution
lines(werte, dnorm(werte), col=2)   #  "-"     normal    "-"

plot(werte, punif(werte, -1, 1), type="l") # probability distribution: uniform 
lines(werte, pnorm(werte), col=2)   #        pd: normal (cumulative probability)

# Plotten von Zufallszahlen einer Gleichverteilung: runif(n, von, bis)
# alle Werte (zwischen 0 und 1) sind gleich wahrscheinlich (uniform)
plot(runif(10))    #   Random numbers from UNIForm distribution
plot(runif(1000), pch=16, cex=0.5)


# Hier eine Auswahl weiterer Verteilungen, mit Dank an Boris Schrˆder:
rnorm(n, mean=0, sd=1)          # Gaussian (normal)
rexp(n, rate=1)                 # exponential
rgamma(n, shape, scale=1)       # gamma
rpois(n, lambda)                # Poisson
rweibull(n, shape, scale=1)     # Weibull
rcauchy(n, location=0, scale=1) # Cauchy
rbeta(n, shape1, shape2)        # beta
rt(n, df)                       # 'Student' (t)
rf(n ,df1, df2)                 # Fisher-Snedecor (F)
rchisq(n, df)                   # Pearson (Chi^2)
rbinom(n, size, prob)           # binomial
rgeom(n, prob)                  # geometric
rhyper(nn, m, n, k)             # hypergeometric
rlogis(n, location=0, scale=1)  # logistic
rlnorm(n, meanlog=0, sdlog=1)   # lognormal
rnbinom(n, size, prob)          # negative binomial
runif(n, min=0, max=1)          # uniform
rwilcox(nn, m, n)               # Wilcoxon's statistics
rsignrank(nn, n)                # Wilcoxon's statistics

browseURL("http://cran.r-project.org/doc/manuals/R-intro.pdf")
# Kapitel 8 Probability distributions  (2013-03-01 Seite 41 PDF / 35 print)

browseURL("www.bb-sbl.de/tutorial/verteilungen")

# Eine ganze Reihe (Extremwert-)verteilungen an Daten fitten und analysieren:
# package extremeStat (on github), siehe Kapitel 15.7



# 5.7 Simulationen -------------------------------------------------------------
# Wenn man nicht berechnen kann oder will, kann man oft auch simulieren.
# z.B. die Verteilung der Summenzahl dreier W¸rfeln:
f <- function() sample(1:6, 1e7, replace=TRUE)
erg <- f() + f()+ f() # Vektorisieren, wann immer es geht
plot(table(erg)/1e7, las=1, main="empirische Wahrscheinlichkeit
f¸r erg Anzahl Augen mit 3 fairen W¸rfeln")

# z.B. die Wahrscheinlichkeit, drei M‰dchen hintereinander zu bekommen:
gender <- t(sapply(1:1e5, function(x) sample(0:1, 3, TRUE) ))
head(gender)
plot(table(rowSums(gender))/1e5, las=1, main="Anzahl M‰dchen bei 3 Kindern")
# gilt nur, wenn
# - Geschlecht von dem der Geschwister unabh‰ngig ist
# - die Wahrscheinlichkeit f¸r ein Geschlecht 50% betr‰gt
# - kein Survivorship Bias besteht (M‰dchen ˆfter abgetrieben als Jungs zB)
# - ...
