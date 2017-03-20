### RclickHandbuch.wordpress.com ### Berry Boessenkool ### berry-b@gmx.de ######
### Kapitel 14: Regression (Quadratmittelapproximation) ########################

# 14.1 lineare Regression (Anpassung einer Gerade an Wertepaare)
# 14.2 Geradengleichung ermitteln und verwenden
# 14.3 logarithmische Regression
# 14.4 multivariate Regression
# 14.5 cubic regression
# 14.6 Funktion


# 14.1 lineare Regression (Anpassung einer Gerade an Wertepaare) ---------------
x <- c(5,8,10,9,13,6,2) ; y <- c(567,543,587,601,596,533,512)
plot(x,y, las=1, pch=16)
modellgerade <- lm(y~x)  # y abh. von x.  xy-Reihenfolge anders als sonst meist!
modellgerade
?formula

# lm(y~x): lineares Modell mit y abhängig von x
# y = a*x + b,  a = Steigung (slope), b = Konstante, Achsenabschnitt (intercept)
# y: abhängige (dependent) Variable ,    criterion / response
# x: erklärende unabhängige (independent) Variable , predictor


summary(modellgerade)
# Zur Interpretation siehe zB:
browseURL("http://blog.yhathq.com/posts/r-lm-summary.html")
browseURL("http://stats.stackexchange.com/questions/5135/interpretation-of-rs-lm-output")

# Siehe 9.1 Hypothesentests Allgemein
# Nullhypothese beim Linearen Modell: Zusammenhang zufällig ("Rauschen")
# Also ist p die Wahrscheinlichkeit, dass der Parameter (die Variable)
# - das Modell nicht verbessert (multivariate Regression)
# - und nicht zur Erklärung von y relevant ist.
# Wenn p < alpha (oft = 0.05): Variable drin lassen. Es gibt einen Grund für die
# Korrelation, auch wenn R^2 gering ist ("Signal")
# Ziel LinMod: R2 hoch, Anzahl Parameter klein (Parsimonie prinzip, aka
# Ockhams Razor: so viele Variablen wie nötig, so wenige wie möglich).

str(modellgerade) # sämtliche Info, die im Objekt enthalten ist!
abline(modellgerade, lwd=3, col=4)
cor(x, y)

# A high correlation does NOT imply correct causation!
# the strong negative correlation childbirth ~ stork abundancy does not mean
# getting more storks to breed here will help us fight demographic change ;-)


cov(x,y)/(sd(x)*sd(y)) # Kovarianz durch Standardabweichungen
cor(x, y)^2 # = R^2
summary(modellgerade)$r.square # 0.6685065

p <- predict(modellgerade)
p # Achtung, für jeweilige x-Punkte, aber in aufsteigender Reihenfolge!
points(x, predict(modellgerade), col=2, pch=16)

# Beim lm wird die Summe der Distanzen zum Quadrat minimiert:
segments(x, y, x, p)
# oder umständlich: for (i in 1:7) lines(  x=c(x[i], x[i]), y=c(y[i], p[i])  )
text(x, (y+p)/2, round(y-p,1), adj=-0.2)

# Die Linien stellen die Residuen (Restfehler) dar
residuals(modellgerade)
sum((y-p)^2) # 2328.884   SSE (Sum of Squared Errors) oder RSS: (Residual SS)

sum((p-mean(y))^2)/sum((y-mean(y))^2) # Variation Regresswerte / Variation Y
1 - sum((y-p)^2)/sum((y-mean(y))^2)

# Unsicherheit:
plot(x,y, las=1, pch=16)  ; abline(modellgerade, lwd=3, col=4)
x2 <- seq(1, 14, length=30)
p2 <- predict(modellgerade, newdata=data.frame(x=x2), interval="confidence" )
polygon(x=c(x2, rev(x2)), y=c(p2[,2], rev(p2[,3])), col=rgb(0.3,0.5,0, 0.5))

# Einfluss:
plot(x,y, las=1, pch=16, cex=influence(modellgerade)$hat*5)
abline(modellgerade, lwd=3, col=4)
influence(modellgerade)
influence.measures(modellgerade)

op <- par(mfrow=c(1,2), mar=c(3.5,3.5,3,0.5), mgp=c(2.1, 0.7,0), las=1)
data(cars)
mod <- lm(dist ~ speed, cars)
plot(dist ~ speed, cars, pch=16, cex=influence(mod)$hat*10,
     main="cars regression:\ninfluence(mod)$hat")
abline(mod)
infl <- function(x,y)
  {
  slope_all <- coef(lm(y~x))[2]
  slope_i <- sapply(seq_along(x), function(i) coef(lm(y[-i]~x[-i]))[2]  )
  abs(slope_i - slope_all)
  }
plot(dist ~ speed, cars, pch=16, cex=infl(cars$speed, cars$dist)*10,
     main="Slope change if\npoint is left out")
abline(mod)
par(op)
browseURL("https://onlinecourses.science.psu.edu/stat501/node/337")



# ANOVA   # mehr zu ANOVA im Kapitel 9.5
anova(modellgerade)
summary(anova(modellgerade))



# 14.2 Geradengleichung ermitteln und verwenden --------------------------------
# bei jedem ersten Durchlauf an jedem Rechner gleiche "Zufallszahlen"
set.seed(12)
x <- c(runif(100,0,3), runif(200, 3, 25)) # random aus uniformer Verteilung
y <- 12.367*log10(x)+7.603+rnorm(300)     # random aus Normalverteilung


plot(x, y, las=1, pch=20, main="Verdeutlichung lin.regr.  vs.  log.regr.")

# Koeffizienten des linearen Regressionsmodells bestimmen
linmod <- lm( y ~ x )
linmod
str(linmod) # Liste mit 12 Elementen
abline(linmod, col=2, lwd=3)
summary(linmod)
r <- round( summary(linmod)$r.squared ,2)
m <- round( coef(linmod)[2] ,2 )    # runde die zweite Koefiziente auf 2 Stellen
b <- round( coef(linmod)[1] ,2 )
Txt <- paste("y =", m, "* x +", b, "\nR\U00B2 =", r)  #\n macht Zeilenumbruch
Txt
text(12,10, Txt, adj=0, col=2)

# Predict
predict(linmod) # Standardmäßig für Werte, an denen gefitted wurde
predict(linmod, newdata=data.frame(x=seq(0,20,by=4)))
# colnames() des Dataframes muss genau den Namen enthalten, der auch für
# lm verwendet wurde...



# 14.3 logarithmische Regression -----------------------------------------------
# Koeffizienten des logarithmischen Regressionsmodells bestimmen
# Achtung: log berechnet ln (Log zur Basis e), log10 den dekadischen Logarithmus
# Negative Werte ersetzen, sonst können wir  keinen Logarithmus berechnen
# x <- replace(x, x <= 0, 0.0001)  # in den Beispieldaten nicht nötig
# ist eine Veränderung der Daten und daher nur begründet anzuwenden!
set.seed(12)
x <- c(runif(100,0,3), runif(200, 3, 25)) # random aus uniformer Verteilung
y <- 12.367*log10(x)+7.603+rnorm(300)     # random aus Normalverteilung
plot(x,y)

logmod <- lm( y ~ log10(x) )   ; summary(logmod)
r2 <- round( summary(logmod)$r.squared ,2)
m2 <- round( coef(logmod)[2] ,2 )
b2 <- round( coef(logmod)[1] ,2 )
xvekt <- seq(0,35,0.01)
lines(xvekt, m2*log10(xvekt)+b2, col=4, lwd=3)
text(12,5, paste("y=", m2, "* log(x) +", b2, "\nR\U00B2 =", r2), adj=0, col=4)



# 14.4 multivariate Regression -------------------------------------------------
# Regression mit mehreren erklärenden Variablen A,B,C
# Syntax (Pseudo-code): lm(y ~ A + B + C)
#mit Transformationen müsste sowas gehen: lm( y ~ A + I(log(B)) + C)

browseURL("http://dl.dropboxusercontent.com/u/4836866/Sonstiges/lakes.txt")
lakes <- read.table("Daten/14_lakes.txt", header=T )
pairs(lakes)

textcor <- function(x,y)
           legend("center", legend=round(cor(x,y),2), cex=2, bty="n", adj=0.4)
pairs(lakes, lower.panel=textcor)

summary(  lm(chlorophyll ~ P,            data=lakes) )$r.squared # 0.77
summary(  lm(chlorophyll ~ P + N + temp, data=lakes) )$r.squared # 0.79
# R^2 etwas höher, aber:
summary(  lm(chlorophyll ~ P + N + temp, data=lakes) )
# Von 3 Variablen nur einer, der signifikant einen Anteil der Varianz erklärt.
# Nach Ockhams Razor (Sparsamkeitsprinzip) werden die anderen beiden weggelassen
# "Steht man vor der Wahl mehrerer möglicher Erklärungen für dasselbe Phänomen,
# soll man diejenige bevorzugen, die mit der geringsten Anzahl an Hypothesen
# auskommt und somit die 'einfachste' Theorie darstellt." [Wikipedia]
# Also sukzessive die Variable mit dem höchsten P-value rauslassen,
# dabei die Änderung des R2 etwas mit im Blick behalten.
# Siehe Hinweis Zeile 30.

cor(lakes$N, lakes$P) # erklärende Variablen sind untereinander korreliert.

pnt_mod <- lm(chlorophyll ~ P + N + temp, data=lakes)
pnt_mod
# chlorophyll = 0.34*P + 1.14*N + 2.97*t - 33.7
K <- coef(pnt_mod)
K
K[2] # ist ein Vector, auch wenn es wie ein data.frame aussieht
class(K) # numeric

# Schätze die Chlorophyllkonzentration für eine Messung mit P=100, N=15, t=9
K[1] + 100*K[2] + 15*K[3] + 9*K[4] # 44
# zeigt als Namen den ersten Namen von k an, daher
as.vector(   K[1] + 100*K[2] + 15*K[3] + 9*K[4]  )

View(data.frame(lakes, Chl_ESTIMATE=0.3409*P + 1.1403*N + 2.9687*temp -33.7333))


# Random Forest - wichtigkeit einzelner Prädiktoren
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value means more important:
round(importance(ozone.rf), 2)
# Correlation matrix:
round(abs(cor(airquality, use="complete")), 2)
# compare cor and imp
correlation <- c(0.35,0.61,0.70,0.14,0.01)
importance <- c(11.97,23.22,45.22,2.10,2.83)
berryFunctions::linReg(correlation, importance)



# 14.5 cubic regression --------------------------------------------------------
a <- c(-5:7, 10)  ;  b <- 5*a^3 + 2*a^2 - 7*a + 3 + rnorm(14, 0, 100)
data.frame(a,b) ; plot(a,b)

modell <- lm(b ~  poly(a,3, raw=T))  ;  modell  # gleich mehr zu raw
lines(a, predict(modell))
xdraw <- seq(3, 11, length=1e4)
lines(xdraw, predict(modell, data.frame(a=xdraw)), col=2)
coef(modell)
dummy.coef(modell)

poly(a,3, raw=T)
poly(a,3)  # Beim Default-Wert von Raw (FALSE) wird normiert!

i <- seq (-5,10,0.1)   ;   j <-  3*i^2 - 5*i + 4
lm(j ~ poly(i,2))       # kommt was falsches bei rum
lm(j ~ poly(i,2,raw=T)) # jetzt richtig


# eine andere Möglichkeit, "+" auch für multivariate Regression:
lm(b ~ I(a^3) + I(a^2) + a)



# 14.6 Funktion ----------------------------------------------------------------
# Mit Funktion Werte in die Graphik schreiben (toll für Rprofile oder Paket)
linreg <- function(x,y, pos="top", new=TRUE, pch=20, col=2, lwd=3, inset=0, ...)
      {     if (new) plot(x, y, las=1, pch=pch, ...)
      mod <- lm( y ~ x )    
      abline(mod, col=col, lwd=lwd, ...)
      r <- round( summary(mod)$r.squared ,2)
      m <- round( coef(mod)[2] ,2 )
      b <- round( coef(mod)[1] ,2 )
      Txt <- paste0("y = ", m ," * x ", if(b>0)" + ", b, "\nR\U00B2 = ", r)
      legend(pos, Txt, bty="n", text.col=col, inset=inset)  }
# linreg(x, y, new=FALSE) fügt die Werte einem bestehenden (!) Plot hinzu
# Wäre leicht um ein digits-Argument für die Nachkommastellen zu erweitern.

a <- 1:30
b <- a/2.345+rnorm(30,0,3)

linreg(a,b, ylab="Hallo", pch=1, col=3)

plot(a,b, xlim=c(-5,45))
linreg(a, b, pos="bottomright", new=F, inset=.1) # inset für Abstand vom Rand
