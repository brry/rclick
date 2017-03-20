### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 6: Lagemaße, Boxplot, Histogramm, qq-Plot ##########################
# (exploratory data analysis, EDA)  

# 6.1 Daten
# 6.2 Histogramme
# 6.3 Lagemaße
# 6.4 Boxplots (und warum man sie nicht so oft verwenden sollte)
# 6.5 QQ-Plots


# 6.1 Daten --------------------------------------------------------------------
# hydraulische Leitfähigkeit (Ksat) in Tiefe A und B
set.seed(28) # set.seed sorgt dafür, dass jeder Lauf die gleichen Zahlen ergibt.
KsA <- round(rexp(40, .02), digits=2)
KsB <- round(rexp(40, .04), digits=2)
KsA  # Hier habe ich 40 Zufallszahlen der Exponentialverteilung gewählt.
KsB  
Ks <- data.frame(KsA, KsB)  ; Ks



# 6.2 Histogramme --------------------------------------------------------------
hist(KsA, col=3, breaks=10)
# zwischen 0 und 20 liegen 15 Werte, usw.
histdata <- hist(KsB, col=2)
histdata
rm(histdata) # wieder aus dem Workspace löschen

par(mfrow=c(1,2))
hist(KsA, cex.axis=0.7, breaks=20, col=3)
hist(KsB, cex.axis=0.7, col=2)
par(mfrow=c(1,1))
# Oder in einem Plot:
hist(KsA, border=3, col=rgb(0,1,0, alpha=.2)) # Alpha für Teiltransparenz
hist(KsB, border=2, col=rgb(1,0,0, alpha=.2), add=T); box()
legend("topright", c("KsA","KsB"), fill=rgb(0:1,1:0,0, alpha=.2), border=3:2 )

# Auswirkung der Anzahl Breaks:
?volcano
hist(volcano, breaks=5,   main="5",   ylim=c(0,2000), xlim=c(50,200))
hist(volcano, breaks=10,  main="10",  ylim=c(0,2000), xlim=c(50,200))
hist(volcano, breaks=15,  main="15",  ylim=c(0,2000), xlim=c(50,200))
hist(volcano, breaks=40,  main="40",  ylim=c(0,2000), xlim=c(50,200))
hist(volcano, breaks=80,  main="80",  ylim=c(0,2000), xlim=c(50,200))
hist(volcano, breaks=150, main="150", ylim=c(0,2000), xlim=c(50,200))

# Rand nur außenrum:
h <- hist(volcano, col=3, border=NA)
polygon(x=c(rep(h$breaks, each=2)),  y=c(0, rep(h$counts, each=2), 0), lwd=2 )



# 6.3 Lagemaße -----------------------------------------------------------------
# Minimalwert, Maximalwert, Wertespanne
min(KsA)
max(KsA)
range(KsA)

# arithmetisches Mittel vs Median
mean(KsA) # 54 mm/h
median(KsA) # 50% der Werte liegt unter 32 mm/h, 50% der Messwerte sind größer
# Der Mittelwert wird von den wenigen sehr hohen Werten hochgezogen.
# Der Median ist ausreißerunabhängig. (Wobei hohe Ksatwerte keine Messfehler 
# sind, also nicht wirklich als Ausreißer zu bezeichnen sind.)

# Beide Maßzahlen für die mittlere Größe einer Variable haben auch 
# ein Maß für die Streuung: Standardabweichung (sd) und median Abweichung (mad)
sd(KsA) # 60

# Mit mean und sd kann jeder die Verteilung nachvollziehen:
install.packages("berryFunctions"); library("berryFunctions")
normPlot(mean(KsA), sd(KsA), ylim=c(0, 0.02), cum=F)
hist(KsA, breaks=10, freq=F, add=T, col=2)
# Die Verteilung wird mit den beiden Parametern völlig falsch beschrieben.
# Mittelwert und Stabw sind NUR für normalverteilte Daten sinnvoll (siehe Kap 9)

# MAD: Median der absoluten Abweichungen vom Median (median absolute deviation)
# wird normalerweise normiert; damit das nicht passiert: constant = 1
mad(KsA, constant=1) # durchnittlich liegen die Werte 24 mm vom median entfernt.

# Quantile = Percentile, x% der Werte unter Quantil
quantile(KsA)
# 75% der Werte liegt unter 70 mm/h, dennoch gibt es Werte bis zu 296!
# 25, 50 und 75% werden Quartile (Viertelbereiche) genannt, Q1, Q2 und Q3

# InterQuartilsDistanz: Abstand zwischen erstem (25%) und drittem (75%) Quartil
IQR(KsA) # IQR = Inter Quartil Range, auch Fourth-spread
# 50 % der Werte liegen zwischen Q1 und Q3 mit einer Breite von 54 mm/h 
# IQR entspricht der Streungsbreite ohne Extremwerte. Im Boxplot (siehe 6.4) 
# wird ein Wert als Ausreißer gesehen, wenn er 1.5*IQR über Q3 / unter Q1 liegt.

# Fünf-ZahlenMaß: Min, 1.Quartil, Median, 3.Quartil, Max
fivenum(KsA)
?boxplot.stats # Section Details erklärt, warum quantile und fivenum Q1 und Q3
# unterschiedlich herausgeben

# Spalten vergleichen: Übersicht einiger Kennzahlen
summary(KsA)

# 9-Zahlenmaß: Median, Quartile (Fourth), Oktile (Eighth), Hexadezile, Range
quantile(KsA, prob=c(0, 1/16, 1/8, 1/4, 1/2, 3/4, 7/8, 15/16, 1))

# Quantile kann man mit mehreren Methoden abschätzen:
quantile(KsA, prob=c(0.90, 0.95, 0.99, 1), type=1)
quantile(KsA, prob=c(0.90, 0.95, 0.99, 1), type=2)
# in Kap 11.5 gibts eine systematische Analyse

# EMPIRISCHE kumulative Verteilungsfunktion
plot(ecdf(KsA), verticals=T, do.points=F)



# 6.4 Boxplots -----------------------------------------------------------------
# Auch Box-Whisker-Plot
# Whiskers: Maximalwert oder, wenn Ausreisser vorhanden, 1.5*IQR (ungefähr 2*sd)
boxplot(KsA) # sind aber nicht wirklich Ausreißer, also lieber:
boxplot(KsA, range=0)
axis(1, c(.5,1,1.5)) # um zu entscheiden, wo Text, Pfeile, etc hin soll.
arrows(.7, 100, .98, 160, col=2, lwd=2)

plot(sort(KsA), pch=16); points(sort(KsB), col=2, pch=22)          
Ks <- data.frame(KsA, KsB)  ; head(Ks)
boxplot(Ks, range=0)

# Notch (5%-Vertrauensintervall des Median, bei Statistikern umstritten!)
boxplot(KsA, notch=T, xlab="KsA", ylab="Ksat [mm/h]", col=3, range=0)
# Warnmeldung: einige notches liegen außerhalb der hinges ('box'):
# evtl. notch=FALSE setzen  -> zeigt, dass Daten breit gestreut sind.

# Mittelwert in Boxplot einzeichnen
boxplot(mean(KsA), add=T, border="red")     # add=T: add boxplot to current plot
# Zeichnet einen weiteren Boxplot, bestehend aus nur einem Wert, hinein.

# Lagemaße einzeichnen
abline(h=quantile(KsA, prob=c(1/16,1/8,1/4,1/2,3/4,7/8,15/16)), col ="blue")

# Mehrere Datensätze in ein Fenster boxplotten
boxplot(KsA, KsB, .8*KsA^1.2, notch=T, col=2:4)
boxplot(Ks, notch=T, col=2:3)  # automatische Beschriftung an der x-Achse

# Mittelwerte in mehreren Boxen einzeichnen (mit Sternchen)
a <- as.data.frame(matrix(rnorm(8*5), nrow=8, ncol=5))
bp <- boxplot(a)
m <- apply(a, 2, mean) # wende die funktion mean spaltenweise (2) an. --> Kap.11
# Falls nötig: na.rm=T hinter mean: remove missing data (NA)
points(1:5, m, pch=8, col="red", cex=1.5)

# Um auch das mit Boxplots hinzuzufügen, muss m ein data.frame sein.
m ; class(m); is.vector(m) # Sieht aus wie ein dataframe, ist aber ein Vektor!
ma <- as.data.frame( t(m) )     # t = transpose: Zeile <-> Spalte
ma ; class(ma)
boxplot(ma, add=TRUE, border="red")       

# Spalten vergleichen: Boxplots
boxplot(KsA, KsB, names=c("Ks A", "Ks B"), notch = T,
        ylab = "Ks [mm/h]", col = "grey", main = c("Ks der beiden Tiefen",""))
# Mittelwert addiert
boxplot(data.frame(t(apply(Ks, 2, mean))), add=TRUE, border=2, names=c("",""),
      main=c("","arithmetisches Mittel"), col.main="red", cex.main=0.8, notch=T)


# Allerdings verstecken Boxplots die zugrundeliegende Verteilung der Daten.
# Man sieht nur den Median und die Quartile, graphisch sehr ansprechend, aber
# zur Untersuchung von Datenverteilungen ungeeignet. Siehe dieses Beispiel:
set.seed(007) ; dontuseboxplots <- rbeta(100, shape1=2, shape2=4)
X11() # Ordentliches Graphikfenster, falls du in Rstudio arbeitest
op <- par(mfrow=c(3,1))
boxplot(dontuseboxplots, horizontal=T)
title(main="eine leicht rechtsschiefe Normalverteilung, richtig?")
hist(dontuseboxplots, breaks=20, col=8, main="")
title(main="Nichts da: innerhalb des Boxes sind die Werte fast gleichverteilt!")
plot(density(dontuseboxplots), xlim=range(dontuseboxplots), main="")
title(main="Spannend ist auch eine KernDichteSchätzung")
# oder dieses:
par(mfrow=c(4,1), mar=c(2,2,1,1))
set.seed(42) ; dub1 <- c(rnorm(800,5,1), rnorm(200,9,0.5))
dub2 <- rbeta(1000, 6, 50)*39+1.2 # dub: Don't Use Boxplots (unwisely)
boxplot(dub2, col=4, notch=T, ylim=c(1,12), horizontal=T)
boxplot(dub1, col=3, notch=T, ylim=c(1,12), horizontal=T)
title(main="Die sind sehr änhlich, nicht wahr?", line=-1, adj=0.9)
hist(dub2, breaks=30, xlim=c(1,12), col=4, main=""); box()
hist(dub1, breaks=30, xlim=c(1,12), col=3, main=""); box()
title(main="Oh, doch nicht.", line=-1, adj=0.9)
par(op)
# Boxplots machen nur Sinn, wenn man mehr als zwei Datensätze mit gleicher
# zugrundeliegender Verteilung in einem Plot vergleichen möchte.



# 6.5 QQ-Plots -----------------------------------------------------------------
# qqnorm: Vergleich Datensatz zur Normalverteilung
qqnorm(KsA, main = "Ks_A") 
qqline(KsA)   # Punkte weit weg von der Linie --> Nicht normalverteilt
# Gängig für Ksat-Werte, liegt hier natürlich auch an der Wahl der Daten
plot(sort(KsA))  # das ist letztendlich, was qqnorm macht... 

qqnorm(KsB, main = "Ks_B")
qqline(KsB)

qqplot(KsA, KsB) # qqplot: Vergleich zweier (Datensätze) gegeneinander

# Ich verwende eher Histogramme mit möglichst vielen breaks (in den einzelnen 
# Klassen sollten trotzdem viele Werte drin sein), um zu sehen, ob Daten
# normalverteilt sind.
# Siehe auch Tests auf Normalverteilungen in Kap 9.
