### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 8: Bayes-Statistik und -Hypothesentests ############################

# 8.1 Paket: Bolstad
# 8.2 Urnenmodell
# 8.3
# 8.4 Wasserqualität
# 8.5 Hypothesentests


# 8.1 Paket: Bolstad -----------------------------------------------------------
# R > Pakete > Installiere Paket(e)... > CRAN > Bolstad
# oder kurz: 
install.packages("Bolstad") # Paket Downloaden, entpacken, installieren
# Danach auskommentieren, muss nur einmal ausgeführt werden!
library(Bolstad)    # lädt das Paket, muss in jeder Sitzung geschehen
# Für Binomial sampling with a discrete prior steht folgende Funktion bereit:
?binodp(x,n, pi=NULL, pi.prior=NULL, n.pi=10)



# 8.2 Urnenmodell --------------------------------------------------------------
# Binomialverteilung, diskreter Prior
# n = 5 Kugeln rot/grün; 6 gleichwahrsch. Hypothesen über Anzahl roter Kugeln
# 1 rote gezogen (Erfolg) -> Funktion binodp (Seite 5 Bolstad Manual)
binodp(1, 1, pi = rep(1/6, 6), pi.prior=NULL, n.pi=6 )



# 8.3 Mittelwertschätzung ------------------------------------------------------
# Inferenz: Schluß von einer Stichprobe auf die Grundgesamtheit
# Schätzung des Mittelwertes unter Berücksichtigung von Vorwissen / Schätzung
# Berechnung der Likelihood und des Posteriors eines jeden Wertes
# 5 Hypothesen des Mittelwertes des Gewichts von Studentinnen in Potsdam
mu <- c(55, 60, 64, 66, 73) # mu-Hypothesen
prior <- rep(0.2, 5) # jede ist gleichwahrscheinlich richtig
stichprobe <- c(72, 65, 60, 71, 70, 60, 59)

Bolst <- normdp(x=stichprobe, sigma.x=sd(stichprobe), mu=mu, mu.prior=prior)
# Plot nicht vergessen anzuschauen ;-)
Bolst
round(Bolst$posterior*100,2) #  0.00  2.71 45.62 51.58  0.09
# Gegeben diese Stichprobe, dass sind die Wahrscheinlichkeiten, dass die
# jeweilige Hypothese richtig ist.

# In diesem Fall würde man in der Realität kontinuierlich rechnen lassen:
normgcp(x=stichprobe, sigma.x=sd(stichprobe), density="uniform",
        params=c(45,80), n.mu=80)
# Der Posterior wird nur für n.mu diskrete Werte ausgegeben.



# 8.4 Wasserqualität -----------------------------------------------------------
# Water Quality: n=116 water samples; y=17 samples contaminated.
# Binomialverteilung (n=116, pi)      (siehe Kap. 5)
# pi = true probability that a water sample is contaminated
# posterior distribution of pi given y with a beta(l,4) prior for pi
binobp(17, 116, a=1, b=4)
binobp # zeigt die zugrundeliegende Funktion (Siehe Kap 12)

# Frequentist estimator (Erfolgswahrscheinlichkeit) =  Zahl Erfolge / Samples
(  pi.f <- 17/116  )       # = 0.09

# normal approximation to the posterior distribution g(pi|y).
werte <- seq(0,1, len=10000)
plot(werte ,dnorm(werte, mean=0.1487603, sd=0.0322173), type="l", ylim=c(0,14),
main="n=116;  y=17;  beta(1,4)-prior", ylab="Density", xlab="pi")

# 95% credible interval for pi with the normal approximation:
abline(v=qnorm(c(0.025, 0.975), mean=0.1487603, sd=0.0322173), col=2)
text(0.5, 10, "95% Interval with normal approx", col=2)
# and with the binobp-approach:          
abline(v=c(0.0913896, 0.2171069), col=3)
text(0.45, 8, "95% Interval with binobp", col=3)
                                        
# Ergebnistabelle abspeichern (Datei wird im Working Directory angelegt),  
getwd() # zeigt dieses an 
sink(file="bayes-output.txt", append=T , split=T)
# append=T: outputs werden nicht überschrieben, sondern angehängt
# split=T : output wird auch in der console noch angezeigt

# Benutzung eines anderen Priors: Betrachte Mittelwert und Varianz. 
binobp(17, 116, a = 1,   b = 4)
binobp(17, 116, a = 0.5, b = 1)
binobp(17, 116, a = 1,   b = 0.5)
binobp(17, 116, a = 1,   b = 1)
binobp(17, 116, a = 2,   b = 1)
binobp(17, 116, a = 3,   b = 2)

# Beispiel für weitere Daten
binobp(10, 174, a=1, b=4)

# Schreiben in Datei beenden
sink()


# Posterior distribution g(pi|y) mit beta(1,lO) als prior für pi
# Ziehungen = 116,  Erfolge = 17, Misserfolge = 116-17 = 99
#  a' = a.neu = a.alt + Zahl.Erfolge   b'= b.neu = b.alt + Zahl.Misserfolge
# hier: a.neu =    1  +   17               b.neu = 10    + 116 - 17
binobp(17, 116, a=1,  b=10)
binobp(17, 116, a=18, b=109)  # posterior-beta-Funktion beta.neu = beta(18, 109)

# 95% credible interval for pi: R-Ausgabe Quantile 0.025, 0.975, oder
qbeta(c(0.025, 0.975), 18, 109)

# Vergleich mit Normalverteilung mit gleichem Mittelwert / Standardabweichung
qnorm(c(0.025, 0.975), mean= 0.1440329, sd= 0.0224784)
lines(0:100/100, dnorm(0:100/100, mean=0.1440329, sd=0.0224784), col=3, lty=2)



# 8.5 Hypothesentests ----------------------------------------------------------

#  H0: pi = .l0            H1: pi != .10       Level of significance: 5%
# Nullhypothesen-Wert pi= .10 liegt im Wahrscheinlichkeitsintervall
# Nullhypothese wird nicht verworfen 

# H0: pi>= .15    H1: pi < .15       Level of significance: 5%
# kumulierte Wahrscheinlichkeitsverteilung (pbeta) der posterior-beta-Verteilung
# Gesucht ist der Wert, der in der Wahrscheinlichkeitsdichte-Verteilung der
# Fläche unter der Kurve rechts des Wertes 0.15 entspricht. In der kumulierten
# Wahrscheinlichkeitsverteilung bedeutet dies 1-(Wert bei 0.15)
1-pbeta(.15, shape1= 18, shape2=109)
# Dieser Wert 0.37 ist größer als das Signifikanzlevel 0.05
# Die Null-Hypothese kann nicht verworfen werden


